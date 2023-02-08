use std::borrow::Borrow;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use clvm_rs::allocator::Allocator;
use num_bigint::ToBigInt;

use crate::classic::clvm::__type_compatibility__::{bi_one, bi_zero};
use crate::classic::clvm_tools::stages::stage_0::TRunProgram;

use crate::compiler::clvm::run;
use crate::compiler::codegen::codegen;
use crate::compiler::compiler::is_at_capture;
use crate::compiler::comptypes::{
    Binding, BindingPattern, BodyForm, CompileErr, CompileForm, CompilerOpts, HelperForm, LetData,
    LetFormKind,
};
use crate::compiler::frontend::frontend;
use crate::compiler::runtypes::RunFailure;
use crate::compiler::sexp::decode_string;
use crate::compiler::sexp::SExp;
use crate::compiler::srcloc::Srcloc;
use crate::compiler::stackvisit::{HasDepthLimit, VisitedMarker};
use crate::util::{number_from_u8, u8_from_number, Number};

const PRIM_RUN_LIMIT: usize = 1000000;
pub const EVAL_STACK_LIMIT: usize = 100;

// Governs whether Evaluator expands various forms.
#[derive(Debug, Clone)]
pub struct ExpandMode {
    pub functions: bool,
    pub lets: bool,
}

impl Default for ExpandMode {
    fn default() -> Self {
        ExpandMode {
            functions: true,
            lets: true,
        }
    }
}

// Stack depth checker.
#[derive(Clone, Debug, Default)]
pub struct VisitedInfo {
    functions: HashMap<Vec<u8>, Rc<BodyForm>>,
    max_depth: Option<usize>,
}

impl HasDepthLimit<Srcloc, CompileErr> for VisitedInfo {
    fn depth_limit(&self) -> Option<usize> {
        self.max_depth
    }
    fn stack_err(&self, loc: Srcloc) -> CompileErr {
        CompileErr(loc, "stack limit exceeded".to_string())
    }
}

trait VisitedInfoAccess {
    fn get_function(&mut self, name: &[u8]) -> Option<Rc<BodyForm>>;
    fn insert_function(&mut self, name: Vec<u8>, body: Rc<BodyForm>);
}

impl<'info> VisitedInfoAccess for VisitedMarker<'info, VisitedInfo> {
    fn get_function(&mut self, name: &[u8]) -> Option<Rc<BodyForm>> {
        if let Some(ref mut info) = self.info {
            info.functions.get(name).cloned()
        } else {
            None
        }
    }

    fn insert_function(&mut self, name: Vec<u8>, body: Rc<BodyForm>) {
        if let Some(ref mut info) = self.info {
            info.functions.insert(name, body);
        }
    }
}

// Frontend evaluator based on my fuzzer representation and direct interpreter of
// that.
#[derive(Debug)]
pub enum ArgInputs {
    Whole(Rc<BodyForm>),
    Pair(Rc<ArgInputs>, Rc<ArgInputs>),
}

impl ArgInputs {
    pub fn to_sexp(&self) -> Rc<SExp> {
        match self {
            ArgInputs::Whole(bf) => bf.to_sexp(),
            ArgInputs::Pair(a, b) => Rc::new(SExp::Cons(
                Srcloc::start("*args*"),
                a.to_sexp(),
                b.to_sexp(),
            )),
        }
    }
}

pub struct Evaluator {
    opts: Rc<dyn CompilerOpts>,
    runner: Rc<dyn TRunProgram>,
    prims: Rc<HashMap<Vec<u8>, Rc<SExp>>>,
    helpers: Vec<HelperForm>,
    mash_conditions: bool,
    ignore_exn: bool,
    disable_calls: bool,
}

fn select_helper(bindings: &[HelperForm], name: &[u8]) -> Option<HelperForm> {
    for b in bindings.iter() {
        if b.name() == name {
            return Some(b.clone());
        }
    }

    None
}

fn compute_paths_of_destructure(
    bindings: &mut Vec<(Vec<u8>, Rc<BodyForm>)>,
    structure: Rc<SExp>,
    path: Number,
    mask: Number,
    bodyform: Rc<BodyForm>,
) {
    match structure.atomize() {
        SExp::Cons(_, a, b) => {
            let next_mask = mask.clone() * 2_u32.to_bigint().unwrap();
            let next_right_path = mask + path.clone();
            compute_paths_of_destructure(bindings, a, path, next_mask.clone(), bodyform.clone());
            compute_paths_of_destructure(bindings, b, next_right_path, next_mask, bodyform);
        }
        SExp::Atom(_, name) => {
            let mut produce_path = path.clone() | mask;
            let mut output_form = bodyform.clone();

            while produce_path > bi_one() {
                if path.clone() & produce_path.clone() != bi_zero() {
                    // Right path
                    output_form = Rc::new(make_operator1(
                        &bodyform.loc(),
                        "r".to_string(),
                        output_form,
                    ));
                } else {
                    // Left path
                    output_form = Rc::new(make_operator1(
                        &bodyform.loc(),
                        "f".to_string(),
                        output_form,
                    ));
                }

                produce_path /= 2_u32.to_bigint().unwrap();
            }

            bindings.push((name, output_form));
        }
        _ => {}
    }
}

fn update_parallel_bindings(
    bindings: &HashMap<Vec<u8>, Rc<BodyForm>>,
    have_bindings: &[Rc<Binding>],
) -> HashMap<Vec<u8>, Rc<BodyForm>> {
    let mut new_bindings = bindings.clone();
    for b in have_bindings.iter() {
        match &b.pattern {
            BindingPattern::Name(name) => {
                new_bindings.insert(name.clone(), b.body.clone());
            }
            BindingPattern::Complex(structure) => {
                let mut computed_getters = Vec::new();
                compute_paths_of_destructure(
                    &mut computed_getters,
                    structure.clone(),
                    bi_zero(),
                    bi_one(),
                    b.body.clone(),
                );
                for (name, p) in computed_getters.iter() {
                    new_bindings.insert(name.clone(), p.clone());
                }
            }
        }
    }
    new_bindings
}

// Tell whether the bodyform is a simple primitive.
pub fn is_primitive(expr: &BodyForm) -> bool {
    matches!(
        expr,
        BodyForm::Quoted(_)
            | BodyForm::Value(SExp::Nil(_))
            | BodyForm::Value(SExp::Integer(_, _))
            | BodyForm::Value(SExp::QuotedString(_, _, _))
    )
}

pub fn make_operator1(l: &Srcloc, op: String, arg: Rc<BodyForm>) -> BodyForm {
    BodyForm::Call(
        l.clone(),
        vec![
            Rc::new(BodyForm::Value(SExp::atom_from_string(l.clone(), &op))),
            arg,
        ],
    )
}

pub fn make_operator2(l: &Srcloc, op: String, arg1: Rc<BodyForm>, arg2: Rc<BodyForm>) -> BodyForm {
    BodyForm::Call(
        l.clone(),
        vec![
            Rc::new(BodyForm::Value(SExp::atom_from_string(l.clone(), &op))),
            arg1,
            arg2,
        ],
    )
}

// For any arginput, give a bodyform that computes it.  In most cases, the
// bodyform is extracted, in a few cases, we may need to form a cons operation.
fn get_bodyform_from_arginput(l: &Srcloc, arginput: &ArgInputs) -> Rc<BodyForm> {
    match arginput {
        ArgInputs::Whole(bf) => bf.clone(),
        ArgInputs::Pair(a, b) => {
            let bfa = get_bodyform_from_arginput(l, a);
            let bfb = get_bodyform_from_arginput(l, b);
            Rc::new(make_operator2(l, "c".to_string(), bfa, bfb))
        }
    }
}

// Given an SExp argument capture structure and SExp containing the arguments
// constructed for the function, populate a HashMap with minimized expressions
// which match the requested argument destructuring.
//
// It's possible this will result in irreducible (unknown at compile time)
// argument expressions.
fn create_argument_captures(
    argument_captures: &mut HashMap<Vec<u8>, Rc<BodyForm>>,
    formed_arguments: &ArgInputs,
    function_arg_spec: Rc<SExp>,
) -> Result<(), CompileErr> {
    match (formed_arguments, function_arg_spec.borrow()) {
        (_, SExp::Nil(_)) => Ok(()),
        (ArgInputs::Whole(bf), SExp::Cons(l, f, r)) => {
            match (is_at_capture(f.clone(), r.clone()), bf.borrow()) {
                (Some((capture, substructure)), BodyForm::Quoted(SExp::Cons(_, _, _))) => {
                    argument_captures.insert(capture, bf.clone());
                    create_argument_captures(argument_captures, formed_arguments, substructure)
                }
                (None, BodyForm::Quoted(SExp::Cons(_, fa, ra))) => {
                    // Argument destructuring splits a quoted sexp that can itself
                    // be destructured.
                    let fa_borrowed: &SExp = fa.borrow();
                    let ra_borrowed: &SExp = ra.borrow();
                    create_argument_captures(
                        argument_captures,
                        &ArgInputs::Whole(Rc::new(BodyForm::Quoted(fa_borrowed.clone()))),
                        f.clone(),
                    )?;
                    create_argument_captures(
                        argument_captures,
                        &ArgInputs::Whole(Rc::new(BodyForm::Quoted(ra_borrowed.clone()))),
                        r.clone(),
                    )
                }
                (Some((capture, substructure)), bf) => {
                    argument_captures.insert(capture, Rc::new(bf.clone()));
                    create_argument_captures(argument_captures, formed_arguments, substructure)
                }
                (None, bf) => {
                    // Argument destructuring splits a value that couldn't
                    // previously be reduced.  We'll punt it back unreduced by
                    // specifying how the right part is reached.
                    create_argument_captures(
                        argument_captures,
                        &ArgInputs::Whole(Rc::new(make_operator1(
                            l,
                            "f".to_string(),
                            Rc::new(bf.clone()),
                        ))),
                        f.clone(),
                    )?;
                    create_argument_captures(
                        argument_captures,
                        &ArgInputs::Whole(Rc::new(make_operator1(
                            l,
                            "r".to_string(),
                            Rc::new(bf.clone()),
                        ))),
                        r.clone(),
                    )
                }
            }
        }
        (ArgInputs::Pair(af, ar), SExp::Cons(l, f, r)) => {
            if let Some((capture, substructure)) = is_at_capture(f.clone(), r.clone()) {
                let bfa = get_bodyform_from_arginput(l, af);
                let bfb = get_bodyform_from_arginput(l, ar);
                let fused_arguments = Rc::new(make_operator2(l, "c".to_string(), bfa, bfb));
                argument_captures.insert(capture, fused_arguments);
                create_argument_captures(argument_captures, formed_arguments, substructure)
            } else {
                create_argument_captures(argument_captures, af, f.clone())?;
                create_argument_captures(argument_captures, ar, r.clone())
            }
        }
        (ArgInputs::Whole(x), SExp::Atom(_, name)) => {
            argument_captures.insert(name.clone(), x.clone());
            Ok(())
        }
        (ArgInputs::Pair(_, _), SExp::Atom(l, name)) => {
            argument_captures.insert(
                name.clone(),
                get_bodyform_from_arginput(l, formed_arguments),
            );
            Ok(())
        }
        (_, _) => Err(CompileErr(
            function_arg_spec.loc(),
            format!(
                "not yet supported argument alternative: ArgInput {formed_arguments:?} SExp {function_arg_spec}"
            ),
        )),
    }
}

fn arg_inputs_primitive(arginputs: Rc<ArgInputs>) -> bool {
    match arginputs.borrow() {
        ArgInputs::Whole(bf) => is_primitive(bf),
        ArgInputs::Pair(a, b) => arg_inputs_primitive(a.clone()) && arg_inputs_primitive(b.clone()),
    }
}

pub fn build_argument_captures(
    l: &Srcloc,
    arguments_to_convert: &[Rc<BodyForm>],
    args: Rc<SExp>,
) -> Result<HashMap<Vec<u8>, Rc<BodyForm>>, CompileErr> {
    let mut formed_arguments = ArgInputs::Whole(Rc::new(BodyForm::Quoted(SExp::Nil(l.clone()))));

    for i_reverse in 0..arguments_to_convert.len() {
        let i = arguments_to_convert.len() - i_reverse - 1;
        formed_arguments = ArgInputs::Pair(
            Rc::new(ArgInputs::Whole(arguments_to_convert[i].clone())),
            Rc::new(formed_arguments),
        );
    }

    let mut argument_captures = HashMap::new();
    create_argument_captures(&mut argument_captures, &formed_arguments, args)?;
    Ok(argument_captures)
}

fn make_prim_call(l: Srcloc, prim: Rc<SExp>, args: Rc<SExp>) -> Rc<SExp> {
    Rc::new(SExp::Cons(l, prim, args))
}

pub fn build_reflex_captures(captures: &mut HashMap<Vec<u8>, Rc<BodyForm>>, args: Rc<SExp>) {
    match args.borrow() {
        SExp::Atom(l, name) => {
            captures.insert(
                name.clone(),
                Rc::new(BodyForm::Value(SExp::Atom(l.clone(), name.clone()))),
            );
        }
        SExp::Cons(l, a, b) => {
            if let Some((capture, substructure)) = is_at_capture(a.clone(), b.clone()) {
                captures.insert(
                    capture.clone(),
                    Rc::new(BodyForm::Value(SExp::Atom(l.clone(), capture))),
                );
                build_reflex_captures(captures, substructure);
            } else {
                build_reflex_captures(captures, a.clone());
                build_reflex_captures(captures, b.clone());
            }
        }
        _ => {}
    }
}

pub fn dequote(l: Srcloc, exp: Rc<BodyForm>) -> Result<Rc<SExp>, CompileErr> {
    match exp.borrow() {
        BodyForm::Quoted(v) => Ok(Rc::new(v.clone())),
        _ => Err(CompileErr(
            l,
            format!(
                "not a quoted result in macro expansion: {} {:?}",
                exp.to_sexp(),
                exp
            ),
        )),
    }
}

/*
fn show_env(env: &HashMap<Vec<u8>, Rc<BodyForm>>) {
    let loc = Srcloc::start(&"*env*".to_string());
    for kv in env.iter() {
        println!(
            " - {}: {}",
            SExp::Atom(loc.clone(), kv.0.clone()).to_string(),
            kv.1.to_sexp().to_string()
        );
    }
}
*/

pub fn first_of_alist(lst: Rc<SExp>) -> Result<Rc<SExp>, CompileErr> {
    match lst.borrow() {
        SExp::Cons(_, f, _) => Ok(f.clone()),
        _ => Err(CompileErr(lst.loc(), format!("No first element of {lst}"))),
    }
}

pub fn second_of_alist(lst: Rc<SExp>) -> Result<Rc<SExp>, CompileErr> {
    match lst.borrow() {
        SExp::Cons(_, _, r) => first_of_alist(r.clone()),
        _ => Err(CompileErr(lst.loc(), format!("No second element of {lst}"))),
    }
}

fn synthesize_args_inner(
    template: Rc<SExp>,
    env: &HashMap<Vec<u8>, Rc<BodyForm>>,
) -> Result<Rc<BodyForm>, CompileErr> {
    match template.borrow() {
        SExp::Atom(_, name) => env.get(name).map(|x| Ok(x.clone())).unwrap_or_else(|| {
            Err(CompileErr(
                template.loc(),
                format!("Argument {template} referenced but not in env"),
            ))
        }),
        SExp::Cons(l, f, r) => {
            if let Some((capture, _substructure)) = is_at_capture(f.clone(), r.clone()) {
                synthesize_args_inner(Rc::new(SExp::Atom(l.clone(), capture)), env)
            } else {
                Ok(Rc::new(BodyForm::Call(
                    l.clone(),
                    vec![
                        Rc::new(BodyForm::Value(SExp::atom_from_string(template.loc(), "c"))),
                        synthesize_args_inner(f.clone(), env)?,
                        synthesize_args_inner(r.clone(), env)?,
                    ],
                )))
            }
        }
        SExp::Nil(l) => Ok(Rc::new(BodyForm::Quoted(SExp::Nil(l.clone())))),
        _ => Err(CompileErr(
            template.loc(),
            format!("unknown argument template {template}"),
        )),
    }
}

fn synthesize_args(
    template: Rc<SExp>,
    env: &HashMap<Vec<u8>, Rc<BodyForm>>,
    with_left_env: bool,
) -> Result<Rc<BodyForm>, CompileErr> {
    let env_synth = synthesize_args_inner(template, env)?;
    if with_left_env {
        let loc = env_synth.loc();
        Ok(Rc::new(make_operator2(
            &loc,
            "c".to_string(),
            Rc::new(BodyForm::Quoted(SExp::Nil(loc.clone()))),
            env_synth,
        )))
    } else {
        Ok(env_synth)
    }
}

fn reflex_capture(name: &[u8], capture: Rc<BodyForm>) -> bool {
    match capture.borrow() {
        BodyForm::Value(SExp::Atom(_, n)) => n == name,
        _ => false,
    }
}

fn match_atom_to_prim(name: Vec<u8>, p: u8, h: Rc<SExp>) -> bool {
    match h.borrow() {
        SExp::Atom(_, v) => v == &name || (v.len() == 1 && v[0] == p),
        SExp::Integer(_, v) => *v == p.to_bigint().unwrap(),
        _ => false,
    }
}

fn is_quote_atom(h: Rc<SExp>) -> bool {
    match_atom_to_prim(vec![b'q'], 1, h)
}

fn is_apply_atom(h: Rc<SExp>) -> bool {
    match_atom_to_prim(vec![b'a'], 2, h)
}

fn is_i_atom(h: Rc<SExp>) -> bool {
    match_atom_to_prim(vec![b'i'], 3, h)
}

fn is_cons_atom(h: Rc<SExp>) -> bool {
    match_atom_to_prim(vec![b'c'], 4, h)
}

fn match_cons(args: Rc<BodyForm>) -> Option<(Rc<BodyForm>, Rc<BodyForm>)> {
    if let BodyForm::Call(_, v) = args.borrow() {
        if v.len() < 3 {
            return None;
        }
        let have_cons_atom = is_cons_atom(v[0].to_sexp());
        if have_cons_atom {
            return Some((v[1].clone(), v[2].clone()));
        }
    }

    None
}

fn promote_args_to_bodyform(
    head: Rc<SExp>,
    arg: Rc<SExp>,
    whole_args: Rc<BodyForm>,
) -> Result<Vec<Rc<BodyForm>>, CompileErr> {
    if let Some(v) = arg.proper_list() {
        let head_borrowed: &SExp = head.borrow();
        let mut result = vec![Rc::new(BodyForm::Value(head_borrowed.clone()))];
        for a in v.iter() {
            result.push(promote_program_to_bodyform(
                Rc::new(a.clone()),
                whole_args.clone(),
            )?);
        }
        return Ok(result);
    }

    Err(CompileErr(
        arg.loc(),
        "improper argument list for primitive".to_string(),
    ))
}

fn recognize_consed_env(args_program: Rc<BodyForm>) -> Option<(Rc<BodyForm>, Rc<BodyForm>)> {
    if let BodyForm::Call(_l, args) = args_program.borrow() {
        if args.len() != 3 {
            return None;
        }

        if let BodyForm::Value(SExp::Atom(_, name)) = args[0].borrow() {
            if name[0] == b'c' || name[0] == 4 {
                return Some((args[1].clone(), args[2].clone()));
            }
        }
    }

    None
}

fn choose_from_env_by_path(path_: Number, args_program: Rc<BodyForm>) -> Rc<BodyForm> {
    let mut path = path_;
    let mut op_list = Vec::new();
    let two = 2_i32.to_bigint().unwrap();

    if path == bi_zero() {
        return Rc::new(BodyForm::Quoted(SExp::Nil(args_program.loc())));
    }

    while path != bi_one() {
        op_list.push(path.clone() % two.clone() == bi_one());
        path = path.clone() / two.clone();
    }

    let mut result_form = args_program.clone();
    for op in op_list.iter() {
        if let Some((head, tail)) = match_cons(result_form.clone()) {
            if *op {
                result_form = tail.clone();
            } else {
                result_form = head.clone();
            }
        } else {
            let apply_op = if *op { 6 } else { 5 };
            result_form = Rc::new(BodyForm::Call(
                args_program.loc(),
                vec![
                    Rc::new(BodyForm::Value(SExp::Atom(
                        args_program.loc(),
                        vec![apply_op],
                    ))),
                    result_form,
                ],
            ));
        }
    }
    result_form
}

fn promote_program_to_bodyform(
    program: Rc<SExp>,
    env: Rc<BodyForm>,
) -> Result<Rc<BodyForm>, CompileErr> {
    match program.borrow() {
        SExp::Cons(_, h, t) => {
            if is_quote_atom(h.clone()) {
                let t_borrowed: &SExp = t.borrow();
                return Ok(Rc::new(BodyForm::Quoted(t_borrowed.clone())));
            }

            // Process tails to change bare numbers to (@ n)
            let args = promote_args_to_bodyform(h.clone(), t.clone(), env)?;
            Ok(Rc::new(BodyForm::Call(program.loc(), args)))
        }
        SExp::Integer(_, n) => {
            // A program that is an atom refers to a position
            // in the environment.
            Ok(choose_from_env_by_path(n.clone(), env))
        }
        SExp::QuotedString(_, _, v) => {
            // Treated as integer path.
            let integer = number_from_u8(v);
            Ok(choose_from_env_by_path(integer, env))
        }
        SExp::Atom(_, v) => {
            // Treated as integer path.
            let integer = number_from_u8(v);
            Ok(choose_from_env_by_path(integer, env))
        }
        _ => {
            let borrowed_program: &SExp = program.borrow();
            Ok(Rc::new(BodyForm::Quoted(borrowed_program.clone())))
        }
    }
}

fn match_i_op(candidate: Rc<BodyForm>) -> Option<(Rc<BodyForm>, Rc<BodyForm>, Rc<BodyForm>)> {
    if let BodyForm::Call(_, cvec) = candidate.borrow() {
        if cvec.len() != 4 {
            return None;
        }

        if let BodyForm::Value(atom) = cvec[0].borrow() {
            if is_i_atom(Rc::new(atom.clone())) {
                return Some((cvec[1].clone(), cvec[2].clone(), cvec[3].clone()));
            }
        }
    }

    None
}

fn flatten_expression_to_names_inner(collection: &mut HashSet<Vec<u8>>, expr: Rc<SExp>) {
    match expr.borrow() {
        SExp::Cons(_, a, b) => {
            flatten_expression_to_names_inner(collection, a.clone());
            flatten_expression_to_names_inner(collection, b.clone());
        }
        SExp::Atom(_, a) => {
            collection.insert(a.clone());
        }
        _ => {}
    }
}

fn flatten_expression_to_names(expr: Rc<SExp>) -> Rc<BodyForm> {
    let mut collection = HashSet::new();
    flatten_expression_to_names_inner(&mut collection, expr.clone());
    let mut transformed = Vec::new();
    for a in collection.iter() {
        transformed.push(a.clone());
    }
    transformed.sort();
    let mut call_vec: Vec<Rc<BodyForm>> = transformed
        .iter()
        .map(|x| Rc::new(BodyForm::Value(SExp::Atom(expr.loc(), x.clone()))))
        .collect();
    call_vec.insert(
        0,
        Rc::new(BodyForm::Value(SExp::Atom(expr.loc(), vec![b'+']))),
    );
    Rc::new(BodyForm::Call(expr.loc(), call_vec))
}

impl<'info> Evaluator {
    pub fn new(
        opts: Rc<dyn CompilerOpts>,
        runner: Rc<dyn TRunProgram>,
        helpers: Vec<HelperForm>,
    ) -> Self {
        Evaluator {
            opts: opts.clone(),
            runner,
            prims: opts.prim_map(),
            helpers,
            mash_conditions: false,
            ignore_exn: false,
            disable_calls: false,
        }
    }

    pub fn disable_calls(&self) -> Self {
        Evaluator {
            opts: self.opts.clone(),
            runner: self.runner.clone(),
            prims: self.prims.clone(),
            helpers: self.helpers.clone(),
            mash_conditions: false,
            ignore_exn: true,
            disable_calls: true,
        }
    }

    pub fn enable_calls_for_macro(&self) -> Self {
        Evaluator {
            opts: self.opts.clone(),
            runner: self.runner.clone(),
            prims: self.prims.clone(),
            helpers: self.helpers.clone(),
            mash_conditions: false,
            ignore_exn: true,
            disable_calls: false,
        }
    }

    pub fn mash_conditions(&self) -> Self {
        Evaluator {
            opts: self.opts.clone(),
            runner: self.runner.clone(),
            prims: self.prims.clone(),
            helpers: self.helpers.clone(),
            mash_conditions: true,
            ignore_exn: true,
            disable_calls: false,
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn invoke_macro_expansion(
        &self,
        allocator: &mut Allocator,
        visited: &'_ mut VisitedMarker<'info, VisitedInfo>,
        l: Srcloc,
        call_loc: Srcloc,
        program: Rc<CompileForm>,
        prog_args: Rc<SExp>,
        arguments_to_convert: &[Rc<BodyForm>],
        env: &HashMap<Vec<u8>, Rc<BodyForm>>,
    ) -> Result<Rc<BodyForm>, CompileErr> {
        // Pass the SExp representation of the expressions into
        // the macro after forming an argument sexp and then
        let mut macro_args = Rc::new(SExp::Nil(l.clone()));
        for i_reverse in 0..arguments_to_convert.len() {
            let i = arguments_to_convert.len() - i_reverse - 1;
            let arg_repr = arguments_to_convert[i].to_sexp();
            macro_args = Rc::new(SExp::Cons(l.clone(), arg_repr, macro_args));
        }

        let macro_expansion = self.expand_macro(allocator, l.clone(), program, macro_args)?;

        if let Ok(input) = dequote(call_loc, macro_expansion.clone()) {
            let frontend_macro_input = Rc::new(SExp::Cons(
                l.clone(),
                Rc::new(SExp::atom_from_string(l.clone(), "mod")),
                Rc::new(SExp::Cons(
                    l.clone(),
                    prog_args.clone(),
                    Rc::new(SExp::Cons(l.clone(), input, Rc::new(SExp::Nil(l)))),
                )),
            ));

            frontend(self.opts.clone(), &[frontend_macro_input]).and_then(|program| {
                self.shrink_bodyform_visited(
                    allocator,
                    visited,
                    prog_args.clone(),
                    env,
                    program.exp,
                    Default::default(),
                )
            })
        } else {
            promote_program_to_bodyform(
                macro_expansion.to_sexp(),
                Rc::new(BodyForm::Value(SExp::Atom(
                    macro_expansion.loc(),
                    vec![b'@'],
                ))),
            )
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn invoke_primitive(
        &self,
        allocator: &mut Allocator,
        visited: &mut VisitedMarker<'info, VisitedInfo>,
        l: Srcloc,
        call_name: &[u8],
        parts: &[Rc<BodyForm>],
        body: Rc<BodyForm>,
        prog_args: Rc<SExp>,
        arguments_to_convert: &[Rc<BodyForm>],
        env: &HashMap<Vec<u8>, Rc<BodyForm>>,
        expand: ExpandMode,
    ) -> Result<Rc<BodyForm>, CompileErr> {
        let mut all_primitive = true;
        let mut target_vec: Vec<Rc<BodyForm>> = parts.to_owned();

        if call_name == "@".as_bytes() {
            // Synthesize the environment for this function
            if parts.is_empty() {
                return Err(CompileErr(l, "@ callable not given a constant".to_string()));
            }

            let path_sexp = match parts[1].borrow() {
                BodyForm::Value(val) => Ok(val),
                BodyForm::Quoted(val) => Ok(val),
                _ => Err(CompileErr(
                    l.clone(),
                    "Not a supported argument for @ call".to_string(),
                )),
            }?;
            let path = if let Some(bigint) = path_sexp.to_bigint() {
                Ok(bigint)
            } else {
                Err(CompileErr(l, "Not a number in @ call".to_string()))
            }?;

            // Choose from the live environment.
            let literal_args = synthesize_args(prog_args.clone(), env, true)?;
            let arg_part = self.shrink_bodyform_visited(
                allocator,
                visited,
                prog_args,
                env,
                literal_args,
                expand,
            )?;

            let chosen = choose_from_env_by_path(path, arg_part);
            Ok(chosen)
        } else if call_name == "com".as_bytes() {
            let mut end_of_list = Rc::new(SExp::Cons(
                l.clone(),
                arguments_to_convert[0].to_sexp(),
                Rc::new(SExp::Nil(l.clone())),
            ));

            for h in self.helpers.iter() {
                end_of_list = Rc::new(SExp::Cons(l.clone(), h.to_sexp(), end_of_list))
            }

            let use_body = SExp::Cons(
                l.clone(),
                Rc::new(SExp::Atom(l.clone(), "mod".as_bytes().to_vec())),
                Rc::new(SExp::Cons(
                    l.clone(),
                    Rc::new(SExp::Cons(l.clone(), Rc::new(SExp::Nil(l)), prog_args)),
                    end_of_list,
                )),
            );

            let compiled = self.compile_code(allocator, false, Rc::new(use_body))?;
            let compiled_borrowed: &SExp = compiled.borrow();
            Ok(Rc::new(BodyForm::Quoted(compiled_borrowed.clone())))
        } else {
            if arguments_to_convert.len() == 1 {
                // Try to short circuit destruct conses.
                let is_first = call_name == b"f" || call_name == vec![5];
                if is_first || call_name == b"r" || call_name == vec![6] {
                    if let Some((first, rest)) =
                        recognize_consed_env(arguments_to_convert[0].clone())
                    {
                        if is_first {
                            let evaluated_first = self.shrink_bodyform_visited(
                                allocator, visited, prog_args, env, first, expand,
                            )?;
                            return Ok(evaluated_first);
                        } else {
                            let evaluated_rest = self.shrink_bodyform_visited(
                                allocator, visited, prog_args, env, rest, expand,
                            )?;
                            return Ok(evaluated_rest);
                        }
                    }
                }
            }

            let pres = self
                .lookup_prim(l.clone(), call_name)
                .map(|prim| {
                    // Reduce all arguments.
                    let mut converted_args = SExp::Nil(l.clone());

                    for i_reverse in 0..arguments_to_convert.len() {
                        let i = arguments_to_convert.len() - i_reverse - 1;
                        let shrunk = self.shrink_bodyform_visited(
                            allocator,
                            visited,
                            prog_args.clone(),
                            env,
                            arguments_to_convert[i].clone(),
                            expand.clone(),
                        )?;

                        target_vec[i + 1] = shrunk.clone();

                        if !arg_inputs_primitive(Rc::new(ArgInputs::Whole(shrunk.clone()))) {
                            all_primitive = false;
                        }

                        converted_args =
                            SExp::Cons(l.clone(), shrunk.to_sexp(), Rc::new(converted_args));
                    }

                    if all_primitive {
                        match self.run_prim(
                            allocator,
                            l.clone(),
                            make_prim_call(l.clone(), prim, Rc::new(converted_args)),
                            Rc::new(SExp::Nil(l.clone())),
                        ) {
                            Ok(res) => Ok(res),
                            Err(e) => {
                                if !expand.functions || self.ignore_exn {
                                    Ok(Rc::new(BodyForm::Call(l.clone(), target_vec.clone())))
                                } else {
                                    Err(e)
                                }
                            }
                        }
                    } else {
                        let reformed = BodyForm::Call(l.clone(), target_vec.clone());
                        self.chase_apply(allocator, visited, Rc::new(reformed))
                    }
                })
                .unwrap_or_else(|| {
                    // Build SExp arguments for external call or
                    // return the unevaluated chunk with minimized
                    // arguments.
                    Err(CompileErr(
                        l.clone(),
                        format!(
                            "Don't yet support this call type {} {:?}",
                            body.to_sexp(),
                            body
                        ),
                    ))
                })?;
            Ok(pres)
        }
    }

    fn continue_apply(
        &self,
        allocator: &mut Allocator,
        visited: &'_ mut VisitedMarker<'info, VisitedInfo>,
        env: Rc<BodyForm>,
        run_program: Rc<SExp>,
    ) -> Result<Rc<BodyForm>, CompileErr> {
        let bindings = HashMap::new();
        let program = promote_program_to_bodyform(run_program.clone(), env)?;
        let apply_result = self.shrink_bodyform_visited(
            allocator,
            visited,
            Rc::new(SExp::Nil(run_program.loc())),
            &bindings,
            program,
            Default::default(),
        )?;
        self.chase_apply(allocator, visited, apply_result)
    }

    fn do_mash_condition(
        &self,
        allocator: &mut Allocator,
        visited: &'_ mut VisitedMarker<'info, VisitedInfo>,
        maybe_condition: Rc<BodyForm>,
        env: Rc<BodyForm>,
    ) -> Result<Rc<BodyForm>, CompileErr> {
        // The inner part could be an 'i' which we know passes on
        // one of the two conditional arguments.  This was an apply so
        // we can distribute over the conditional arguments.
        if let Some((cond, iftrue, iffalse)) = match_i_op(maybe_condition.clone()) {
            let x_head = Rc::new(BodyForm::Value(SExp::Atom(cond.loc(), vec![b'x'])));
            let apply_head = Rc::new(BodyForm::Value(SExp::Atom(iftrue.loc(), vec![2])));
            let where_from = cond.loc().to_string();
            let where_from_vec = where_from.as_bytes().to_vec();

            if let Some(present) = visited.get_function(&where_from_vec) {
                return Ok(present);
            }

            visited.insert_function(
                where_from_vec,
                Rc::new(BodyForm::Call(
                    maybe_condition.loc(),
                    vec![x_head.clone(), cond.clone()],
                )),
            );

            let surrogate_apply_true = self.chase_apply(
                allocator,
                visited,
                Rc::new(BodyForm::Call(
                    iftrue.loc(),
                    vec![apply_head.clone(), iftrue.clone(), env.clone()],
                )),
            );

            let surrogate_apply_false = self.chase_apply(
                allocator,
                visited,
                Rc::new(BodyForm::Call(
                    iffalse.loc(),
                    vec![apply_head, iffalse.clone(), env],
                )),
            );

            // Reproduce the equivalent hull over the used values of
            // (a (i cond surrogate_apply_true surrogate_apply_false))
            // Flatten and short circuit any farther evaluation since we just
            // want the argument names passed through from the environment.
            let res = Rc::new(BodyForm::Call(
                maybe_condition.loc(),
                vec![
                    x_head,
                    flatten_expression_to_names(cond.to_sexp()),
                    flatten_expression_to_names(surrogate_apply_true?.to_sexp()),
                    flatten_expression_to_names(surrogate_apply_false?.to_sexp()),
                ],
            ));

            return Ok(res);
        }

        Err(CompileErr(maybe_condition.loc(), "not i op".to_string()))
    }

    fn chase_apply(
        &self,
        allocator: &mut Allocator,
        visited: &'_ mut VisitedMarker<'info, VisitedInfo>,
        body: Rc<BodyForm>,
    ) -> Result<Rc<BodyForm>, CompileErr> {
        if let BodyForm::Call(l, vec) = body.borrow() {
            if is_apply_atom(vec[0].to_sexp()) {
                if let Ok(run_program) = dequote(l.clone(), vec[1].clone()) {
                    return self.continue_apply(allocator, visited, vec[2].clone(), run_program);
                }

                if self.mash_conditions {
                    if let Ok(mashed) =
                        self.do_mash_condition(allocator, visited, vec[1].clone(), vec[2].clone())
                    {
                        return Ok(mashed);
                    }
                }
            }
        }

        Ok(body)
    }

    #[allow(clippy::too_many_arguments)]
    fn handle_invoke(
        &self,
        allocator: &mut Allocator,
        visited: &'_ mut VisitedMarker<'info, VisitedInfo>,
        l: Srcloc,
        call_loc: Srcloc,
        call_name: &[u8],
        head_expr: Rc<BodyForm>,
        parts: &[Rc<BodyForm>],
        body: Rc<BodyForm>,
        prog_args: Rc<SExp>,
        arguments_to_convert: &[Rc<BodyForm>],
        env: &HashMap<Vec<u8>, Rc<BodyForm>>,
        expand: ExpandMode,
    ) -> Result<Option<Rc<BodyForm>>, CompileErr> {
        let helper = select_helper(&self.helpers, call_name);
        match helper {
            Some(HelperForm::Defmacro(mac)) => self
                .invoke_macro_expansion(
                    allocator,
                    visited,
                    mac.loc.clone(),
                    call_loc,
                    mac.program,
                    prog_args,
                    arguments_to_convert,
                    env,
                )
                .map(Some),
            Some(HelperForm::Defun(inline, defun)) => {
                if !inline && !expand.functions {
                    return Ok(None);
                }

                if self.disable_calls {
                    let mut call_vec = vec![head_expr];
                    for a in arguments_to_convert.iter() {
                        call_vec.push(self.shrink_bodyform_visited(
                            allocator,
                            visited,
                            prog_args.clone(),
                            env,
                            a.clone(),
                            expand.clone(),
                        )?);
                    }
                    return Ok(Some(Rc::new(BodyForm::Call(l, call_vec))));
                }

                let argument_captures_untranslated =
                    build_argument_captures(&call_loc, arguments_to_convert, defun.args.clone())?;

                let mut argument_captures = HashMap::new();
                // Do this to protect against misalignment
                // between argument vec and destructuring.
                for kv in argument_captures_untranslated.iter() {
                    let shrunk = self.shrink_bodyform_visited(
                        allocator,
                        visited,
                        prog_args.clone(),
                        env,
                        kv.1.clone(),
                        expand.clone(),
                    )?;

                    argument_captures.insert(kv.0.clone(), shrunk.clone());
                }

                self.shrink_bodyform_visited(
                    allocator,
                    visited,
                    defun.args.clone(),
                    &argument_captures,
                    defun.body,
                    expand,
                )
                .map(Some)
            }
            _ => self
                .invoke_primitive(
                    allocator,
                    visited,
                    l,
                    call_name,
                    parts,
                    body,
                    prog_args,
                    arguments_to_convert,
                    env,
                    expand,
                )
                .and_then(|res| self.chase_apply(allocator, visited, res))
                .map(Some),
        }
    }

    // A frontend language evaluator and minifier
    pub fn shrink_bodyform_visited(
        &self,
        allocator: &mut Allocator, // Support random prims via clvm_rs
        visited_: &'info mut VisitedMarker<'_, VisitedInfo>,
        prog_args: Rc<SExp>,
        env: &HashMap<Vec<u8>, Rc<BodyForm>>,
        body: Rc<BodyForm>,
        expand: ExpandMode,
    ) -> Result<Rc<BodyForm>, CompileErr> {
        let mut visited = VisitedMarker::again(body.loc(), visited_)?;
        match body.borrow() {
            BodyForm::Let(LetFormKind::Parallel, letdata) => {
                if !expand.lets {
                    return Ok(body.clone());
                }

                let updated_bindings = update_parallel_bindings(env, &letdata.bindings);
                self.shrink_bodyform_visited(
                    allocator,
                    &mut visited,
                    prog_args,
                    &updated_bindings,
                    letdata.body.clone(),
                    expand,
                )
            }
            BodyForm::Let(LetFormKind::Sequential, letdata) => {
                if !expand.lets {
                    return Ok(body.clone());
                }

                if letdata.bindings.is_empty() {
                    self.shrink_bodyform_visited(
                        allocator,
                        &mut visited,
                        prog_args,
                        env,
                        letdata.body.clone(),
                        expand,
                    )
                } else {
                    let first_binding_as_list: Vec<Rc<Binding>> =
                        letdata.bindings.iter().take(1).cloned().collect();
                    let rest_of_bindings: Vec<Rc<Binding>> =
                        letdata.bindings.iter().skip(1).cloned().collect();

                    let updated_bindings = update_parallel_bindings(env, &first_binding_as_list);
                    self.shrink_bodyform_visited(
                        allocator,
                        &mut visited,
                        prog_args,
                        &updated_bindings,
                        Rc::new(BodyForm::Let(
                            LetFormKind::Sequential,
                            LetData {
                                loc: letdata.loc.clone(),
                                kw: letdata.kw.clone(),
                                bindings: rest_of_bindings,
                                body: letdata.body.clone(),
                            },
                        )),
                        expand,
                    )
                }
            }
            BodyForm::Quoted(_) => Ok(body.clone()),
            BodyForm::Value(SExp::Atom(l, name)) => {
                if name == &"@".as_bytes().to_vec() {
                    let literal_args = synthesize_args(prog_args.clone(), env, true)?;
                    self.shrink_bodyform_visited(
                        allocator,
                        &mut visited,
                        prog_args,
                        env,
                        literal_args,
                        expand,
                    )
                } else if let Some(x) = env.get(name) {
                    if reflex_capture(name, x.clone()) {
                        Ok(x.clone())
                    } else {
                        self.shrink_bodyform_visited(
                            allocator,
                            &mut visited,
                            prog_args,
                            env,
                            x.clone(),
                            expand,
                        )
                    }
                } else if let Some(x) = self.get_constant(name, expand.clone()) {
                    self.shrink_bodyform_visited(
                        allocator,
                        &mut visited,
                        prog_args,
                        env,
                        x,
                        expand,
                    )
                } else {
                    Ok(Rc::new(BodyForm::Value(SExp::Atom(
                        l.clone(),
                        name.clone(),
                    ))))
                }
            }
            BodyForm::Value(v) => Ok(Rc::new(BodyForm::Quoted(v.clone()))),
            BodyForm::Call(l, parts) => {
                if parts.is_empty() {
                    return Err(CompileErr(
                        l.clone(),
                        "Impossible empty call list".to_string(),
                    ));
                }

                // Allow us to punt all calls to functions, which preserved type
                // signatures for type checking.
                let head_expr = parts[0].clone();
                let arguments_to_convert: Vec<Rc<BodyForm>> =
                    parts.iter().skip(1).cloned().collect();

                match head_expr.borrow() {
                    BodyForm::Value(SExp::Atom(call_loc, call_name)) => self
                        .handle_invoke(
                            allocator,
                            &mut visited,
                            l.clone(),
                            call_loc.clone(),
                            call_name,
                            head_expr.clone(),
                            parts,
                            body.clone(),
                            prog_args.clone(),
                            &arguments_to_convert,
                            env,
                            expand.clone(),
                        )
                        .and_then(|x| {
                            if let Some(res) = x {
                                Ok(res)
                            } else {
                                let mut converted_arguments = vec![head_expr.clone()];
                                for arg in arguments_to_convert.iter() {
                                    converted_arguments.push(self.shrink_bodyform_visited(
                                        allocator,
                                        &mut visited,
                                        prog_args.clone(),
                                        env,
                                        arg.clone(),
                                        expand.clone(),
                                    )?);
                                }
                                Ok(Rc::new(BodyForm::Call(body.loc(), converted_arguments)))
                            }
                        }),
                    BodyForm::Value(SExp::Integer(call_loc, call_int)) => self
                        .handle_invoke(
                            allocator,
                            &mut visited,
                            l.clone(),
                            call_loc.clone(),
                            &u8_from_number(call_int.clone()),
                            head_expr.clone(),
                            parts,
                            body.clone(),
                            prog_args,
                            &arguments_to_convert,
                            env,
                            expand,
                        )
                        .map(|x| x.unwrap_or_else(|| body.clone())),
                    _ => Err(CompileErr(
                        l.clone(),
                        format!("Don't know how to call {}", head_expr.to_sexp()),
                    )),
                }
            }
            BodyForm::Mod(_, true, program) => {
                // A mod form yields the compiled code.
                let mut have_helpers = HashSet::new();
                for helper in program.helpers.iter() {
                    have_helpers.insert(helper.name().to_vec());
                }
                let mut prog_copy = program.clone();
                for helper in self.helpers.iter() {
                    if !have_helpers.contains(helper.name()) {
                        prog_copy.helpers.push(helper.clone());
                    }
                }
                let code = codegen(
                    allocator,
                    self.runner.clone(),
                    self.opts.clone(),
                    &prog_copy,
                    &mut HashMap::new(),
                )?;
                Ok(Rc::new(BodyForm::Quoted(code)))
            }
            BodyForm::Mod(_, false, program) => {
                // A mod form yields the compiled code.
                let code = codegen(
                    allocator,
                    self.runner.clone(),
                    self.opts.clone(),
                    program,
                    &mut HashMap::new(),
                )?;
                Ok(Rc::new(BodyForm::Quoted(code)))
            }
        }
    }

    pub fn shrink_bodyform(
        &self,
        allocator: &mut Allocator, // Support random prims via clvm_rs
        prog_args: Rc<SExp>,
        env: &HashMap<Vec<u8>, Rc<BodyForm>>,
        body: Rc<BodyForm>,
        expand: ExpandMode,
        stack_limit: Option<usize>,
    ) -> Result<Rc<BodyForm>, CompileErr> {
        let visited_info = VisitedInfo {
            max_depth: stack_limit,
            ..Default::default()
        };
        let mut visited_marker = VisitedMarker::new(visited_info);
        self.shrink_bodyform_visited(
            allocator, // Support random prims via clvm_rs
            &mut visited_marker,
            prog_args,
            env,
            body,
            expand,
        )
    }

    fn expand_macro(
        &self,
        allocator: &mut Allocator, // Support random prims via clvm_rs
        call_loc: Srcloc,
        program: Rc<CompileForm>,
        args: Rc<SExp>,
    ) -> Result<Rc<BodyForm>, CompileErr> {
        let mut end_of_list = Rc::new(SExp::Cons(
            call_loc.clone(),
            program.exp.to_sexp(),
            Rc::new(SExp::Nil(call_loc.clone())),
        ));

        for h in program.helpers.iter() {
            end_of_list = Rc::new(SExp::Cons(call_loc.clone(), h.to_sexp(), end_of_list))
        }

        let use_body = Rc::new(SExp::Cons(
            call_loc.clone(),
            Rc::new(SExp::Atom(call_loc.clone(), "mod".as_bytes().to_vec())),
            Rc::new(SExp::Cons(
                call_loc.clone(),
                program.args.clone(),
                end_of_list,
            )),
        ));

        let compiled = self.compile_code(allocator, false, use_body)?;
        self.run_prim(allocator, call_loc, compiled, args)
    }

    fn lookup_prim(&self, l: Srcloc, name: &[u8]) -> Option<Rc<SExp>> {
        match self.prims.get(name) {
            Some(p) => Some(p.clone()),
            None => {
                if name.len() == 1 {
                    Some(Rc::new(SExp::Atom(l, name.to_owned())))
                } else {
                    None
                }
            }
        }
    }

    fn run_prim(
        &self,
        allocator: &mut Allocator,
        call_loc: Srcloc,
        prim: Rc<SExp>,
        args: Rc<SExp>,
    ) -> Result<Rc<BodyForm>, CompileErr> {
        run(
            allocator,
            self.runner.clone(),
            self.prims.clone(),
            prim,
            args,
            Some(PRIM_RUN_LIMIT),
        )
        .map_err(|e| match e {
            RunFailure::RunExn(_, s) => CompileErr(call_loc.clone(), format!("exception: {s}")),
            RunFailure::RunErr(_, s) => CompileErr(call_loc.clone(), s),
        })
        .map(|res| {
            let res_borrowed: &SExp = res.borrow();
            Rc::new(BodyForm::Quoted(res_borrowed.clone()))
        })
    }

    fn compile_code(
        &self,
        allocator: &mut Allocator,
        in_defun: bool,
        use_body: Rc<SExp>,
    ) -> Result<Rc<SExp>, CompileErr> {
        // Com takes place in the current environment.
        // We can only reduce com if all bindings are
        // primitive.
        let updated_opts = self
            .opts
            .set_stdenv(!in_defun)
            .set_in_defun(in_defun)
            .set_frontend_opt(false);

        let com_result = updated_opts.compile_program(
            allocator,
            self.runner.clone(),
            use_body,
            &mut HashMap::new(),
        )?;

        Ok(Rc::new(com_result))
    }

    pub fn add_helper(&mut self, h: &HelperForm) {
        for i in 0..self.helpers.len() {
            if self.helpers[i].name() == h.name() {
                self.helpers[i] = h.clone();
                return;
            }
        }
        self.helpers.push(h.clone());
    }

    // The evaluator treats the forms coming up from constants as live.
    fn get_constant(&self, name: &[u8], expand: ExpandMode) -> Option<Rc<BodyForm>> {
        for h in self.helpers.iter() {
            if let HelperForm::Defconstant(defc) = h {
                if defc.name == name && (!defc.tabled || expand.functions) {
                    return Some(defc.body.clone());
                }
            }
        }
        None
    }
}
