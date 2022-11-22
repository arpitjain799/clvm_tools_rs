use std::borrow::Borrow;
use std::collections::{BTreeMap, HashMap};
use std::mem::swap;
use std::rc::Rc;

use clvm_rs::allocator;
use clvm_rs::allocator::{Allocator, NodePtr};
use clvm_rs::reduction::EvalErr;
use num_bigint::ToBigInt;

use crate::classic::clvm::__type_compatibility__::{Bytes, BytesFromType, Stream, bi_one, bi_zero};
use crate::classic::clvm::serialize::{sexp_from_stream, SimpleCreateCLVMObject};
use crate::classic::clvm_tools::sha256tree::sha256tree;
use crate::classic::clvm_tools::stages::stage_0::TRunProgram;

use crate::compiler::clvm;
use crate::compiler::clvm::{convert_from_clvm_rs, run_step, RunStep};
use crate::compiler::runtypes::RunFailure;
use crate::compiler::sexp::{SExp, decode_string, parse_sexp};
use crate::compiler::srcloc::Srcloc;
use crate::util::Number;

#[derive(Clone, Debug)]
pub struct PriorResult {
    reference: usize,
    // value: Rc<SExp>, // In future, we'll want to know the value produced.
}

#[derive(Clone, Debug)]
pub struct RunStepRelevantInfo {
    hash: Vec<u8>,
    enter: bool,
    op: Rc<SExp>,
    name: String,
    prog: Rc<SExp>,
    args: Rc<SExp>,
    tail: Rc<SExp>
}

#[derive(Clone, Debug)]
pub struct ComputedArgument {
    pub path: Number,
    pub name: Vec<u8>,
    pub value: Rc<SExp>,
}

#[derive(Clone, Debug)]
pub struct ToComputeArgument {
    path: Number,
    name: Vec<u8>,
    program: Rc<SExp>,
}

pub fn is_op(v: u8, op: Rc<SExp>) -> bool {
    if let SExp::Integer(_, i) = op.borrow() {
        *i == v.to_bigint().unwrap()
    } else if let SExp::Atom(_, n) = op.borrow() {
        *n == vec![v]
    } else {
        false
    }
}

pub fn is_apply_op(op: Rc<SExp>) -> bool { is_op(2, op) }
pub fn is_cons_op(op: Rc<SExp>) -> bool { is_op(4, op) }

fn decode_cons(form: Rc<SExp>) -> Option<(Rc<SExp>, Rc<SExp>)> {
    if let Some(lst) = form.proper_list() {
        if lst.len() == 3 && is_cons_op(Rc::new(lst[0].clone())) {
            return Some((Rc::new(lst[1].clone()), Rc::new(lst[2].clone())));
        }
    }

    None
}

pub fn format_arg_inputs(args: &[PriorResult]) -> String {
    let value_strings: Vec<String> = args.iter().map(|pr| pr.reference.to_string()).collect();
    value_strings.join(", ")
}

pub fn hex_of_hash(hash: &[u8]) -> String {
    Bytes::new(Some(BytesFromType::Raw(hash.to_vec()))).hex()
}

pub fn get_arg_associations(
    associations: &HashMap<Number, PriorResult>,
    args: Rc<SExp>,
) -> Vec<PriorResult> {
    let mut arg_exp: Rc<SExp> = args;
    let mut result: Vec<PriorResult> = Vec::new();
    loop {
        if let SExp::Cons(_, arg, rest) = arg_exp.borrow() {
            if let Some(n) = arg
                .get_number()
                .ok()
                .as_ref()
                .and_then(|n| associations.get(n))
            {
                result.push(n.clone());
            }
            arg_exp = rest.clone();
        } else {
            return result;
        }
    }
}

pub fn get_fun_hash(
    op: Rc<SExp>,
    sexp: Rc<SExp>
) -> Option<(Vec<u8>, Rc<SExp>, Rc<SExp>)> {
    if let SExp::Cons(_, prog, args) = sexp.borrow() {
        if is_apply_op(op.clone()) {
            if let SExp::Cons(_, env, _) = args.borrow() {
                eprintln!("get_fun_hash args {} env {}", args, env);
                return Some((clvm::sha256tree(prog.clone()), prog.clone(), env.clone()));
            }
        }
    }

    None
}

fn isolate_arguments(args: Rc<SExp>, tail: Rc<SExp>, path: Number, mask: Number, skip_first: bool) -> Vec<ComputedArgument> {
    let this_path = path.clone() | mask.clone();
    let name =
        if let SExp::Atom(_, n) = args.borrow() {
            n.clone()
        } else {
            format!("arg_path_{}", this_path).as_bytes().to_vec()
        };

    if let SExp::Cons(_, f, r) = tail.borrow() {
        if skip_first {
            return isolate_arguments(args, r.clone(), path, mask, false);
        }

        if let SExp::Cons(_, a, b) = args.borrow() {
            let next_mask = 2_u32.to_bigint().unwrap() * mask.clone();
            let mut first =
                isolate_arguments(a.clone(), f.clone(), path, next_mask.clone(), false);
            let mut rest =
                isolate_arguments(b.clone(), r.clone(), this_path, next_mask, false);
            first.append(&mut rest);
            return first;
        }
    }

    vec![ComputedArgument {
        path: this_path,
        name: name,
        value: tail.clone()
    }]
}

pub fn relevant_run_step_info(symbol_table: &HashMap<String, String>, step: &RunStep) -> Option<RunStepRelevantInfo> {
    if let RunStep::Step(sexp, ctx, _) = step {
        if let SExp::Cons(_, op, args) = sexp.borrow() {
            return get_fun_hash(op.clone(), args.clone()).map(|(hash, prog, env)| {
                eprintln!("runstep::op {} prog {} env {}", op, prog, env);
                make_relevant_info(
                    symbol_table,
                    op.clone(),
                    &hash,
                    prog,
                    env
                )
            })
        }
    }

    None
}

pub fn capture_argument_generation(
    args: Rc<SExp>,
    path: Number,
    mask: Number,
    tail: Rc<SExp>
) -> Vec<ToComputeArgument> {
    let this_path = path.clone() | mask.clone();
    if let (SExp::Cons(_, a, b), Some((left, right))) = (args.borrow(), decode_cons(tail.clone())) {
        let next_mask = mask.clone() * 2_u32.to_bigint().unwrap();
        let right_path = mask | path.clone();
        let mut left_vec = capture_argument_generation(
            args.clone(), path, next_mask.clone(), left
        );
        let mut right_vec = capture_argument_generation(
            args, right_path, next_mask, right
        );
        left_vec.append(&mut right_vec);
        left_vec
    } else if let SExp::Atom(_, name) = args.borrow() {
        vec![ToComputeArgument {
            path: this_path,
            name: name.clone(),
            program: tail
        }]
    } else {
        // We should compute this, but we'll just call it by a name
        vec![ToComputeArgument {
            name: format!("arg_path_{}", path).as_bytes().to_vec(),
            path: this_path,
            program: tail
        }]
    }
}



pub trait CldbRunnable {
    fn replace_step(&self, step: &RunStep) -> Option<Result<RunStep, RunFailure>>;
}

pub trait CldbEnvironment {
    fn add_context(
        &self,
        s: &SExp,
        c: &SExp,
        args: Option<Rc<SExp>>,
        context_result: &mut BTreeMap<String, String>,
    );
    fn add_function(&self, s: &SExp, context_result: &mut BTreeMap<String, String>);
    fn get_override(&self, s: &RunStep) -> Option<Result<RunStep, RunFailure>>;
}

pub struct CldbRun {
    runner: Rc<dyn TRunProgram>,
    prim_map: Rc<HashMap<Vec<u8>, Rc<SExp>>>,
    env: Box<dyn CldbEnvironment>,

    step: RunStep,

    ended: bool,
    final_result: Option<Rc<SExp>>,
    to_print: BTreeMap<String, String>,
    in_expr: bool,
    row: usize,

    outputs_to_step: HashMap<Number, PriorResult>,
}

impl CldbRun {
    pub fn new(
        runner: Rc<dyn TRunProgram>,
        prim_map: Rc<HashMap<Vec<u8>, Rc<SExp>>>,
        env: Box<dyn CldbEnvironment>,
        step: RunStep,
    ) -> Self {
        CldbRun {
            runner,
            prim_map,
            env,
            step,
            ended: false,
            final_result: None,
            to_print: BTreeMap::new(),
            in_expr: false,
            row: 0,
            outputs_to_step: HashMap::<Number, PriorResult>::new(),
        }
    }

    pub fn is_ended(&self) -> bool {
        self.ended
    }

    pub fn final_result(&self) -> Option<Rc<SExp>> {
        self.final_result.clone()
    }

    pub fn step(&mut self, allocator: &mut Allocator) -> Option<BTreeMap<String, String>> {
        let mut produce_result = false;
        let mut result = BTreeMap::new();
        let new_step = match self.env.get_override(&self.step) {
            Some(v) => v,
            _ => run_step(
                allocator,
                self.runner.clone(),
                self.prim_map.clone(),
                &self.step,
            ),
        };

        // Allow overrides by consumers.

        match &new_step {
            Ok(RunStep::OpResult(l, x, _p)) => {
                if self.in_expr {
                    self.to_print
                        .insert("Result-Location".to_string(), l.to_string());
                    self.to_print.insert("Value".to_string(), x.to_string());
                    self.to_print
                        .insert("Row".to_string(), self.row.to_string());
                    if let Ok(n) = x.get_number() {
                        self.outputs_to_step.insert(
                            n,
                            PriorResult {
                                reference: self.row,
                                // value: x.clone(), // for future
                            },
                        );
                    }
                    self.in_expr = false;
                    swap(&mut self.to_print, &mut result);
                    produce_result = true;
                }
            }
            Ok(RunStep::Done(l, x)) => {
                self.to_print
                    .insert("Final-Location".to_string(), l.to_string());
                self.to_print.insert("Final".to_string(), x.to_string());

                self.ended = true;
                self.final_result = Some(x.clone());
                swap(&mut self.to_print, &mut result);
                produce_result = true;
            }
            Ok(RunStep::Step(_sexp, _c, _p)) => {}
            Ok(RunStep::Op(sexp, c, a, None, _p)) => {
                self.to_print
                    .insert("Operator-Location".to_string(), a.loc().to_string());
                self.to_print
                    .insert("Operator".to_string(), sexp.to_string());
                if let Ok(v) = sexp.get_number() {
                    if v == 11_u32.to_bigint().unwrap() {
                        // Build source tree for hashes.
                        let arg_associations =
                            get_arg_associations(&self.outputs_to_step, a.clone());
                        let args = format_arg_inputs(&arg_associations);
                        self.to_print.insert("Argument-Refs".to_string(), args);
                    }
                }
                self.env.add_context(
                    sexp.borrow(),
                    c.borrow(),
                    Some(a.clone()),
                    &mut self.to_print,
                );
                self.env.add_function(sexp, &mut self.to_print);
                self.in_expr = true;
            }
            Ok(RunStep::Op(_sexp, _c, _a, Some(_v), _p)) => {}
            Err(RunFailure::RunExn(l, s)) => {
                self.to_print
                    .insert("Throw-Location".to_string(), l.to_string());
                self.to_print.insert("Throw".to_string(), s.to_string());

                swap(&mut self.to_print, &mut result);
                self.ended = true;
                produce_result = true;
            }
            Err(RunFailure::RunErr(l, s)) => {
                self.to_print
                    .insert("Failure-Location".to_string(), l.to_string());
                self.to_print.insert("Failure".to_string(), s.to_string());

                swap(&mut self.to_print, &mut result);
                self.ended = true;
                produce_result = true;
            }
        }

        self.step = new_step.unwrap_or_else(|_| self.step.clone());

        if produce_result {
            self.row += 1;
            Some(result)
        } else {
            None
        }
    }

    pub fn current_step(&self) -> RunStep {
        self.step.clone()
    }
}

pub struct CldbNoOverride {}

impl CldbRunnable for CldbNoOverride {
    fn replace_step(&self, _step: &RunStep) -> Option<Result<RunStep, RunFailure>> {
        None
    }
}

impl CldbNoOverride {
    pub fn new() -> Self {
        CldbNoOverride {}
    }

    pub fn new_symbols(_symbol_table: HashMap<String, String>) -> Self {
        CldbNoOverride {}
    }
}

impl Default for CldbNoOverride {
    fn default() -> Self {
        CldbNoOverride::new()
    }
}

// Allow the caller to examine environment and return an expression that
// will be quoted.
pub trait CldbSingleBespokeOverride {
    fn get_override(&self, env: Rc<SExp>) -> Result<Rc<SExp>, RunFailure>;
}

pub struct CldbOverrideBespokeCode {
    symbol_table: HashMap<String, String>,
    overrides: HashMap<String, Box<dyn CldbSingleBespokeOverride>>,
}

impl CldbOverrideBespokeCode {
    pub fn new(
        symbol_table: HashMap<String, String>,
        overrides: HashMap<String, Box<dyn CldbSingleBespokeOverride>>,
    ) -> Self {
        CldbOverrideBespokeCode {
            symbol_table,
            overrides,
        }
    }

    fn find_function_and_override_if_needed(
        &self,
        sexp: Rc<SExp>,
        _c: Rc<SExp>,
        f: Rc<SExp>,
        args: Rc<SExp>,
        p: Rc<RunStep>,
    ) -> Option<Result<RunStep, RunFailure>> {
        let fun_hash = clvm::sha256tree(f);
        let fun_hash_str = Bytes::new(Some(BytesFromType::Raw(fun_hash))).hex();

        self.symbol_table
            .get(&fun_hash_str)
            .and_then(|funname| self.overrides.get(funname))
            .map(|override_fn| {
                override_fn
                    .get_override(args.clone())
                    .map(|new_exp| RunStep::OpResult(sexp.loc(), new_exp, p.clone()))
            })
    }
}

impl CldbRunnable for CldbOverrideBespokeCode {
    fn replace_step(&self, step: &RunStep) -> Option<Result<RunStep, RunFailure>> {
        match step {
            RunStep::Op(sexp, context, arguments, None, parent) => match sexp.borrow() {
                SExp::Integer(_, i) => {
                    if *i == 2_u32.to_bigint().unwrap() {
                        match arguments.borrow() {
                            SExp::Cons(_, first, args) => self
                                .find_function_and_override_if_needed(
                                    sexp.clone(),
                                    context.clone(),
                                    first.clone(),
                                    args.clone(),
                                    parent.clone(),
                                ),
                            _ => None,
                        }
                    } else {
                        None
                    }
                }
                _ => None,
            },
            _ => None,
        }
    }
}

pub struct CldbRunEnv {
    input_file: Option<String>,
    program_lines: Rc<Vec<String>>,
    overrides: Box<dyn CldbRunnable>,
}

impl CldbRunEnv {
    pub fn new(
        input_file: Option<String>,
        program_lines: Rc<Vec<String>>,
        runnable: Box<dyn CldbRunnable>,
    ) -> Self {
        CldbRunEnv {
            input_file,
            program_lines,
            overrides: runnable,
        }
    }

    fn extract_text(&self, l: &Srcloc) -> Option<String> {
        let use_line = if l.line < 1 { None } else { Some(l.line - 1) };
        let use_col = use_line.and(if l.col < 1 { None } else { Some(l.col - 1) });
        let end_col = use_col.map(|c| l.until.as_ref().map(|u| u.col - 1).unwrap_or_else(|| c + 1));
        use_line
            .and_then(|use_line| {
                use_col.and_then(|use_col| end_col.map(|end_col| (use_line, use_col, end_col)))
            })
            .and_then(|coords| {
                let use_line = coords.0;
                let use_col = coords.1;
                let mut end_col = coords.2;

                if use_line >= self.program_lines.len() {
                    None
                } else {
                    let line_text = self.program_lines[use_line].to_string();
                    if use_col >= line_text.len() {
                        None
                    } else if end_col >= line_text.len() {
                        end_col = line_text.len();
                        Some(line_text[use_col..end_col].to_string())
                    } else {
                        Some(line_text[use_col..end_col].to_string())
                    }
                }
            })
    }

    fn whether_is_apply(
        &self,
        s: &SExp,
        collector: &mut BTreeMap<String, String>,
        if_true: &dyn Fn(&mut BTreeMap<String, String>),
        if_false: &dyn Fn(&mut BTreeMap<String, String>),
    ) {
        if let SExp::Integer(_, i) = s {
            if *i == 2_i32.to_bigint().unwrap() {
                if_true(collector);
                return;
            }
        }

        if_false(collector);
    }
}

impl CldbEnvironment for CldbRunEnv {
    fn add_context(
        &self,
        s: &SExp,
        c: &SExp,
        args: Option<Rc<SExp>>,
        context_result: &mut BTreeMap<String, String>,
    ) {
        self.whether_is_apply(
            s,
            context_result,
            &|context_result| match c {
                SExp::Cons(_, a, b) => {
                    context_result.insert("Env".to_string(), a.to_string());
                    context_result.insert("Env-Args".to_string(), b.to_string());
                }
                _ => {
                    context_result.insert("Function-Context".to_string(), c.to_string());
                }
            },
            &|context_result| {
                if let Some(a) = &args {
                    context_result.insert("Arguments".to_string(), a.to_string());
                }
            },
        );
    }

    fn add_function(&self, s: &SExp, context_result: &mut BTreeMap<String, String>) {
        self.whether_is_apply(
            s,
            context_result,
            &|_context_result| {},
            &|context_result| {
                if let Some(name) = self.extract_text(&s.loc()) {
                    if Some(s.loc().file.to_string()) == self.input_file.clone() {
                        context_result.insert("Function".to_string(), name);
                    }
                }
            },
        );
    }

    fn get_override(&self, s: &RunStep) -> Option<Result<RunStep, RunFailure>> {
        self.overrides.replace_step(s)
    }
}

pub fn hex_to_modern_sexp_inner(
    allocator: &mut Allocator,
    symbol_table: &HashMap<String, String>,
    loc: Srcloc,
    program: NodePtr,
) -> Result<Rc<SExp>, EvalErr> {
    let hash = sha256tree(allocator, program);
    let hash_str = hash.hex();
    let srcloc = symbol_table
        .get(&hash_str)
        .map(|f| Srcloc::start(f))
        .unwrap_or_else(|| loc.clone());

    match allocator.sexp(program) {
        allocator::SExp::Pair(a, b) => Ok(Rc::new(SExp::Cons(
            srcloc.clone(),
            hex_to_modern_sexp_inner(allocator, symbol_table, srcloc.clone(), a)?,
            hex_to_modern_sexp_inner(allocator, symbol_table, srcloc, b)?,
        ))),
        _ => convert_from_clvm_rs(allocator, srcloc, program).map_err(|_| {
            EvalErr(
                Allocator::null(allocator),
                "clvm_rs allocator failed".to_string(),
            )
        }),
    }
}

pub fn hex_to_modern_sexp(
    allocator: &mut Allocator,
    symbol_table: &HashMap<String, String>,
    loc: Srcloc,
    input_program: &str,
) -> Result<Rc<SExp>, RunFailure> {
    let input_serialized = Bytes::new(Some(BytesFromType::Hex(input_program.to_string())));

    let mut stream = Stream::new(Some(input_serialized));
    let sexp = sexp_from_stream(allocator, &mut stream, Box::new(SimpleCreateCLVMObject {}))
        .map(|x| x.1)
        .map_err(|_| RunFailure::RunErr(loc.clone(), "Bad conversion from hex".to_string()))?;

    hex_to_modern_sexp_inner(allocator, symbol_table, loc.clone(), sexp).map_err(|_| {
        RunFailure::RunErr(loc, "Failed to convert from classic to modern".to_string())
    })
}

#[derive(Clone, Debug)]
pub enum RunPurpose {
    ComputeArgument,
    Main,
}

pub struct HierarchyFrame {
    purpose: RunPurpose,

    prog: Rc<SExp>,
    env: Rc<SExp>,

    pub function_name: String,
    pub function_arguments: Rc<SExp>,

    arguments_to_compute: Vec<ToComputeArgument>,
    arguments_to_show: Vec<ComputedArgument>,
    run: CldbRun
}

pub struct HierarchialRunner {
    allocator: Allocator,
    runner: Rc<dyn TRunProgram>,
    prim_map: Rc<HashMap<Vec<u8>, Rc<SExp>>>,
    symbol_table: Rc<HashMap<String, String>>,
    pub running: Vec<HierarchyFrame>,
    error: bool,
    input_file: Option<String>,
    program_lines: Rc<Vec<String>>,
    prog: Rc<SExp>
}

#[derive(Clone, Debug)]
pub enum HierarchialStepResult {
    ShapeChange,
    Info(Option<BTreeMap<String, String>>)
}

pub enum RunClass {
    Primitive(Rc<SExp>),
    Application(RunStepRelevantInfo),
}

fn make_relevant_info(
    symbol_table: &HashMap<String, String>,
    op: Rc<SExp>,
    hash: &[u8],
    prog: Rc<SExp>,
    env: Rc<SExp>
) -> RunStepRelevantInfo {
    let hex_hash = hex_of_hash(&hash);
    let fun_name = symbol_table.get(&hex_hash).cloned().unwrap_or_else(|| {
        format!("function_{}", hex_hash)
    });
    let fun_args_name = format!("{}_arguments", hex_hash);
    let fun_args = symbol_table.get(&fun_args_name).and_then(|fun_args| {
        parse_sexp(prog.loc(), fun_args.as_bytes().iter().copied()).map(|p| {
            Some(p[0].clone())
        }).unwrap_or_else(|_| None)
    }).unwrap_or_else(|| {
        let name: Vec<u8> = fun_args_name.as_bytes().to_vec();
        Rc::new(SExp::Atom(prog.loc(), name))
    });
    RunStepRelevantInfo {
        hash: hash.to_vec(),
        enter: true,
        op: op.clone(),
        name: fun_name,
        args: fun_args,
        prog: prog.clone(),
        tail: env.clone()
    }
}

fn does_apply(
    symbol_table: &HashMap<String, String>,
    prog: Rc<SExp>
) -> RunClass {
    eprintln!("does_apply {}", prog);
    if let Some(lst) = prog.proper_list() {
        if lst.len() == 3 && is_apply_op(Rc::new(lst[0].clone())) {
            let op = Rc::new(lst[0].clone());
            let prog = Rc::new(lst[1].clone());
            let env = Rc::new(lst[2].clone());
            let hash = clvm::sha256tree(prog.clone());
            return RunClass::Application(make_relevant_info(
                symbol_table,
                op,
                &hash,
                prog,
                env
            ));
        }
    }

    RunClass::Primitive(prog.clone())
}

fn synthesize_arguments(
    loc: Srcloc,
    args: &[ComputedArgument],
    path: Number,
    mask: Number
) -> Rc<SExp> {
    let this_path = path.clone() | mask.clone();
    if args.is_empty() {
        return Rc::new(SExp::Nil(loc));
    }

    for a in args.iter() {
        if a.path == this_path {
            return a.value.clone();
        }
    }

    let next_mask = 2_u32.to_bigint().unwrap() * mask.clone();
    let filtered_args: Vec<ComputedArgument> =
        args.iter().filter(|a| a.path < next_mask).cloned().collect();
    let right_path = mask | path.clone();
    Rc::new(SExp::Cons(
        loc.clone(),
        synthesize_arguments(
            loc.clone(),
            &filtered_args,
            path,
            next_mask.clone()
        ),
        synthesize_arguments(
            loc,
            &filtered_args,
            right_path,
            next_mask
        )
    ))
}

impl HierarchialRunner {
    pub fn new(
        runner: Rc<dyn TRunProgram>,
        prim_map: Rc<HashMap<Vec<u8>, Rc<SExp>>>,
        input_file: Option<String>,
        program_lines: Rc<Vec<String>>,
        symbol_table: Rc<HashMap<String, String>>,
        prog: Rc<SExp>,
        env: Rc<SExp>,
    ) -> Self {
        let step = clvm::start_step(prog.clone(), env.clone());
        let run = CldbRun::new(
            runner.clone(),
            prim_map.clone(),
            Box::new(CldbRunEnv::new(
                input_file.clone(),
                program_lines.clone(),
                Box::new(CldbNoOverride::new())
            )),
            step
        );

        HierarchialRunner {
            allocator: Allocator::new(),
            runner,
            prim_map,
            symbol_table,
            input_file: input_file.clone(),
            program_lines,
            error: false,
            prog: prog.clone(),

            running: vec![HierarchyFrame {
                purpose: RunPurpose::Main,

                prog: prog.clone(),
                env: env,

                function_name: input_file.unwrap_or_else(|| format!("clvm_program_{}", hex_of_hash(&clvm::sha256tree(prog.clone())))),
                function_arguments: Rc::new(SExp::Nil(prog.loc())),

                arguments_to_compute: Vec::new(),
                arguments_to_show: Vec::new(),

                run
            }]
        }
    }

    pub fn is_ended(&self) -> bool {
        self.running.is_empty() || self.error || self.running.len() == 1 && self.running[0].run.is_ended()
    }

    pub fn step(&mut self) -> Result<HierarchialStepResult, RunFailure> {
        if self.running.is_empty() {
            return Err(RunFailure::RunErr(self.prog.loc(), "no running code".to_string()));
        }

        let mut idx = self.running.len() - 1;
        if let Some(outcome) = self.running[idx].run.final_result() {
            let old_running: HierarchyFrame = self.running.pop().unwrap();
            if let RunPurpose::ComputeArgument = old_running.purpose {
                if self.running.is_empty() {
                    return Err(RunFailure::RunErr(self.prog.loc(), "nothing to return argument computation to".to_string()));
                }

                // Retire one argument, start new argument if needed, otherwise
                // pop to main of the function.
                idx -= 1;
                // let this_run = &mut self.running[idx];
                if self.running[idx].arguments_to_compute.is_empty() {
                    return Err(RunFailure::RunErr(self.prog.loc(), "computing an argument but no remaining argument was available".to_string()));
                }

                // Remove the argument we just computed.
                let last_arg = self.running[idx].arguments_to_compute.len() - 1;
                let this_arg = self.running[idx].arguments_to_compute[last_arg].clone();
                self.running[idx].arguments_to_show.insert(0, ComputedArgument {
                    path: this_arg.path.clone(),
                    name: this_arg.name.clone(),
                    value: outcome.clone()
                });

                self.running[idx].arguments_to_compute.pop();

                if self.running[idx].arguments_to_compute.is_empty() {
                    // If we computed the last argument, synthesize the argument
                    // data back into an env and launch the function.
                    let new_arguments = synthesize_arguments(
                        self.running[idx].prog.loc(),
                        &self.running[idx].arguments_to_show,
                        bi_zero(),
                        bi_one()
                    );
                    self.running[idx].env = new_arguments.clone();
                    let step = clvm::start_step(self.running[idx].prog.clone(), new_arguments);
                    let run = CldbRun::new(
                        self.runner.clone(),
                        self.prim_map.clone(),
                        Box::new(CldbRunEnv::new(
                            self.input_file.clone(),
                            self.program_lines.clone(),
                            Box::new(CldbNoOverride::new())
                        )),
                        step
                    );
                    match does_apply(self.symbol_table.borrow(), self.running[idx].prog.clone()) {
                        RunClass::Application(info) => {
                            // Runs a subprogram.
                            eprintln!("run a subprogram: {}", info.name);
                            let to_compute = capture_argument_generation(
                                info.args.clone(),
                                bi_zero(),
                                bi_one(),
                                info.tail.clone()
                            );

                            self.running.push(HierarchyFrame {
                                purpose: RunPurpose::Main,
                                prog: info.prog,
                                env: Rc::new(SExp::Nil(info.tail.loc())),

                                function_name: info.name,
                                function_arguments: info.args,

                                arguments_to_compute: to_compute,
                                arguments_to_show: Vec::new(),

                                run
                            });
                        }
                        RunClass::Primitive(op) => {
                            // Runs a primitive.
                            eprintln!("run a primitive: {}", op);
                            let step = clvm::start_step(op.clone(), self.running[idx].env.clone());
                            let run = CldbRun::new(
                                self.runner.clone(),
                                self.prim_map.clone(),
                                Box::new(CldbRunEnv::new(
                                    self.input_file.clone(),
                                    self.program_lines.clone(),
                                    Box::new(CldbNoOverride::new())
                                )),
                                step
                            );

                            self.running.push(HierarchyFrame {
                                purpose: RunPurpose::Main,
                                prog: op.clone(),
                                env: self.running[idx].env.clone(),

                                function_name: "op".to_string(),
                                function_arguments: Rc::new(SExp::Atom(op.loc(), b"_".to_vec())),

                                arguments_to_compute: Vec::new(),
                                arguments_to_show: Vec::new(),

                                run
                            });
                        }
                    }
                }
            } else {
                // This function ran another function as its expression form.
                // Its result passes through.
                let mut old_running = self.running.pop().unwrap();
                idx = self.running.len() - 1;
                let this_run = &mut self.running[idx];

                swap(&mut this_run.run, &mut old_running.run);
            }

            Ok(HierarchialStepResult::ShapeChange)
        } else {
            let current_env = self.running[idx].env.clone();
            let current_step = self.running[idx].run.current_step();
            eprintln!("step {}", current_step.sexp());
            if let Some(info) = relevant_run_step_info(
                &self.symbol_table,
                &current_step
            ) {
                eprintln!("function {}", info.name);
                eprintln!("args {}", info.args);
                eprintln!("tail {}", info.tail);

                // If we're passing on the whole environment like when
                // starting the program, we can just pass the whole env.
                let isolated_arguments =
                    capture_argument_generation(
                        info.args.clone(),
                        bi_zero(),
                        bi_one(),
                        info.tail.clone()
                    );

                if isolated_arguments.is_empty() {
                    // Just pass on execution.
                    todo!()
                }

                // Create a frame based on the last argument.
                let nil = Rc::new(SExp::Nil(info.args.loc()));
                let arg_idx = isolated_arguments.len() - 1;
                let last_argument_computation = &isolated_arguments[arg_idx];
                let arg_step =
                    clvm::start_step(last_argument_computation.program.clone(), current_env.clone());
                let arg_run = CldbRun::new(
                    self.runner.clone(),
                    self.prim_map.clone(),
                    Box::new(CldbRunEnv::new(
                        self.input_file.clone(),
                        self.program_lines.clone(),
                        Box::new(CldbNoOverride::new())
                    )),
                    arg_step
                );
                let arg_frame = HierarchyFrame {
                    purpose: RunPurpose::ComputeArgument,

                    prog: last_argument_computation.program.clone(),
                    env: current_env.clone(),

                    function_name: format!("{}_{}", info.name.clone(), decode_string(&last_argument_computation.name)),
                    function_arguments: nil.clone(),

                    arguments_to_compute: Vec::new(),
                    arguments_to_show: Vec::new(),

                    run: arg_run
                };

                // Make an empty frame to repopulate (maybe option here?).
                let step = clvm::start_step(info.prog.clone(), current_env.clone());
                let run = CldbRun::new(
                    self.runner.clone(),
                    self.prim_map.clone(),
                    Box::new(CldbRunEnv::new(
                        self.input_file.clone(),
                        self.program_lines.clone(),
                        Box::new(CldbNoOverride::new())
                    )),
                    step
                );

                self.running.push(HierarchyFrame {
                    purpose: RunPurpose::Main,

                    prog: info.prog.clone(),
                    env: current_env,

                    function_name: info.name.clone(),
                    function_arguments: info.args.clone(),

                    arguments_to_compute: isolated_arguments,
                    arguments_to_show: Vec::new(),

                    run
                });

                self.running.push(arg_frame);

                Ok(HierarchialStepResult::ShapeChange)
            } else {
                // Not final result, we'll step the top of the stack.
                let info = self.running[idx].run.step(&mut self.allocator);
                if let Some(i) = &info {
                    self.error = self.error | i.get("Failure").is_some();
                }
                Ok(HierarchialStepResult::Info(info))
            }
        }
    }
}
