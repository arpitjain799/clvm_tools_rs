use num_bigint::{BigInt, ToBigInt};
use num_traits::ToPrimitive;

use rand::distributions::Standard;
use rand::prelude::*;
use rand::Rng;
use rand::random;
use rand_chacha::ChaCha8Rng;
use std::borrow::Borrow;
use std::cell::RefCell;
use std::cmp::max;
use std::mem::swap;
use std::rc::Rc;

use crate::classic::clvm::__type_compatibility__::{bi_one, bi_zero};
use crate::compiler::clvm::truthy;
use crate::compiler::codegen::create_name_lookup_;
use crate::compiler::sexp::{SExp, enlist, random_atom_name, random_sexp};
use crate::compiler::srcloc::Srcloc;
use crate::compiler::runtypes::RunFailure;
use crate::classic::clvm::__type_compatibility__::{
    Bytes,
    BytesFromType,
    Stream,
    sha256
};
use crate::classic::clvm::casts::{
    bigint_to_bytes_clvm
};
use crate::util::{Number, number_from_u8};

const MIN_ARGLIST: usize = 3;
const MAX_STEPS: usize = 1000;
const MAX_OPS: usize = 50000;
const MAX_LIST_BOUND: usize = 3;
const CURRENT_DIALECT: u32 = 21;
const BINDING_NAME_MIN: usize = 3;

thread_local! {
    pub static SEXP_REGISTRY: RefCell<Vec<Rc<SExp>>> = RefCell::new(Vec::new());
    pub static OP_REGISTRY: RefCell<Vec<Rc<FuzzOperation>>> = RefCell::new(Vec::new());
}

#[derive(Debug, Clone)]
pub struct FuzzBinding {
    pub name: Vec<u8>,
    pub expr: FuzzOperation
}

// We don't actually need all operators here, just a good selection with
// semantics that are distinguishable.
#[derive(Debug, Clone)]
pub enum FuzzOperation {
    Argref(usize),
    Quote(SExp),
    If(Rc<FuzzOperation>,Rc<FuzzOperation>,Rc<FuzzOperation>),
    Multiply(Rc<FuzzOperation>,Rc<FuzzOperation>),
    Sub(Rc<FuzzOperation>,Rc<FuzzOperation>),
    Sha256(Vec<FuzzOperation>),
    Let(Vec<FuzzBinding>,Rc<FuzzOperation>),
    Call(u8,Vec<FuzzOperation>),
}

#[derive(Debug, Clone)]
pub enum ArgListType {
    ProperList(u8),
    Structure(SExp),
}

#[derive(Debug, Clone)]
pub struct FuzzFunction {
    pub inline: bool,
    pub number: u8,
    pub args: ArgListType,
    pub body: FuzzOperation,
}

#[derive(Debug, Clone)]
pub struct FuzzProgram {
    pub args: ArgListType,
    pub functions: Vec<FuzzFunction>,
    pub body: FuzzOperation,
}

#[derive(Debug, Clone)]
pub struct FuzzOldProgram {
    pub program: FuzzProgram
}


fn register_op(op: FuzzOperation) -> Rc<FuzzOperation> {
    let rc = Rc::new(op);
    OP_REGISTRY.with(|r_cell| {
        r_cell.replace_with(|v| {
            if v.len() > MAX_OPS {
                panic!("too many ops");
            }
            let mut work = Vec::new();
            swap(&mut work, v);
            work.push(rc.clone());
            work
        });
    });
    rc
}

fn register_sexp(sexp: SExp) -> Rc<SExp> {
    let rc = Rc::new(sexp);
    SEXP_REGISTRY.with(|r_cell| {
        r_cell.replace_with(|v| {
            if v.len() > MAX_OPS {
                panic!("too many ops");
            }
            let mut work = Vec::new();
            swap(&mut work, v);
            work.push(rc.clone());
            work
        });
    });
    rc
}

fn atom_list(sexp: &SExp) -> Vec<Vec<u8>> {
    match sexp {
        SExp::Nil(_) => vec!(),
        SExp::Atom(_,v) => {
            if v.is_empty() {
                vec!()
            } else {
                vec!(v.clone())
            }
        },
        SExp::QuotedString(_,_,_) => vec!(),
        SExp::Integer(_,_) => vec!(),
        SExp::Cons(_,a,b) => {
            let mut a_vec = atom_list(a.borrow());
            let b_vec = atom_list(b.borrow());
            for b_item in b_vec.iter() {
                a_vec.push(b_item.clone());
            }
            a_vec
        }
    }
}


fn select_argument(num: usize, fun: &FuzzProgram, bindings: &[Vec<FuzzBinding>]) -> (SExp, Option<FuzzOperation>) {
    let args_sexp = fun.args.to_sexp();
    let select_group = (num >> 8) % (bindings.len() + 1);
    if select_group == bindings.len() {
        // Select from arguments
        let arg_list = atom_list(&args_sexp);
        let nil = SExp::Nil(args_sexp.loc());
        if arg_list.is_empty() {
            (
                nil.clone(),
                Some(FuzzOperation::Quote(nil))
            )
        } else {
            let selected_arg = arg_list[num & 0xff % arg_list.len()].clone();
            (
                SExp::Atom(args_sexp.loc(), selected_arg),
                None
            )
        }
    } else {
        // Select a binding group using the second byte,
        let group = &bindings[select_group];
        let select_binding = (num & 0xff) % group.len();
        let selected_binding = &group[select_binding];
        // Select a binding using the first byte.
        (
            SExp::Atom(args_sexp.loc(), selected_binding.name.clone()),
            Some(selected_binding.expr.clone())
        )
    }
}

fn select_call(num: u8, prog: &FuzzProgram) -> (String, FuzzFunction) {
    if prog.functions.len() == 0 {
        panic!("we make programs with at least one function");
    }
    let selected_num = num % prog.functions.len() as u8;
    let selected = &prog.functions[selected_num as usize];
    (format!("fun_{}", selected_num), selected.clone())
}

fn make_operator(op: String, args: Vec<SExp>) -> SExp {
    let loc = Srcloc::start(&"*rng*".to_string());
    let mut result = SExp::Nil(loc.clone());

    for i_reverse in 0..args.len() {
        let i = args.len() - i_reverse - 1;
        result = SExp::Cons(
            loc.clone(),
            register_sexp(args[i].clone()),
            register_sexp(result)
        );
    }

    SExp::Cons(
        loc.clone(),
        register_sexp(SExp::atom_from_string(loc.clone(), &op)),
        register_sexp(result)
    )
}

fn distribute_args(a: ArgListType, fun: &FuzzProgram, bindings: &[Vec<FuzzBinding>], arginputs: &Vec<SExp>, spine: bool, argn: u8) -> (u8, SExp) {
    let loc = Srcloc::start(&"*rng*".to_string());
    match a {
        ArgListType::ProperList(0) => (argn, SExp::Nil(loc.clone())),
        ArgListType::ProperList(n) => {
            let rest_result =
                distribute_args(
                    ArgListType::ProperList(n-1),
                    fun,
                    bindings,
                    arginputs,
                    spine,
                    argn + 1,
                );
            (
                rest_result.0,
                SExp::Cons(
                    loc.clone(),
                    register_sexp(arginputs[argn as usize].clone()),
                    register_sexp(rest_result.1)
                )
            )
        },
        ArgListType::Structure(SExp::Nil(l)) => (argn, SExp::Nil(l.clone())),
        ArgListType::Structure(SExp::Cons(l,a,b)) => {
            let a_borrow: &SExp = a.borrow();
            let b_borrow: &SExp = b.borrow();
            let first_res =
                distribute_args(
                    ArgListType::Structure(a_borrow.clone()),
                    fun,
                    bindings,
                    arginputs,
                    false,
                    argn
                );
            let rest_res =
                distribute_args(
                    ArgListType::Structure(b_borrow.clone()),
                    fun,
                    bindings,
                    arginputs,
                    spine,
                    argn + first_res.0
                );
            let res =
                if spine {
                    SExp::Cons(l.clone(), register_sexp(first_res.1), register_sexp(rest_res.1))
                } else {
                    make_operator(
                        "c".to_string(), vec!(first_res.1, rest_res.1)
                    )
                };
            (
                rest_res.0,
                res
            )
        },
        ArgListType::Structure(_) => {
            if spine {
                distribute_args(
                    ArgListType::ProperList(1),
                    fun,
                    bindings,
                    arginputs,
                    spine,
                    argn
                )
            } else {
                (
                    argn + 1_u8,
                    arginputs[argn as usize].clone()
                )
            }
        }
    }
}

fn random_args(loc: Srcloc, a: ArgListType) -> SExp {
    match a {
        ArgListType::ProperList(0) => SExp::Nil(loc.clone()),
        ArgListType::ProperList(n) => {
            let loc = Srcloc::start("*rng*");
            enlist(loc, (0..n).map(|_| register_sexp(random())).collect())
        },
        ArgListType::Structure(SExp::Nil(l)) => SExp::Nil(l.clone()),
        ArgListType::Structure(SExp::Cons(_,a,b)) => {
            let borrowed_a: &SExp = a.borrow();
            let borrowed_b: &SExp = b.borrow();
            SExp::Cons(
                loc.clone(),
                register_sexp(random_args(loc.clone(), ArgListType::Structure(borrowed_a.clone()))),
                register_sexp(random_args(loc.clone(), ArgListType::Structure(borrowed_b.clone())))
            )
        },
        ArgListType::Structure(_) => {
            let random_64: u64 = random();
            SExp::Integer(loc.clone(), random_64.to_bigint().unwrap())
        }
    }
}

impl FuzzOperation {
    pub fn to_sexp(&self, fun: &FuzzProgram, bindings: &[Vec<FuzzBinding>]) -> SExp {
        let loc = Srcloc::start(&"*rng*".to_string());
        match self {
            FuzzOperation::Argref(argument_num) => {
                let argument = select_argument(
                    *argument_num as usize, fun, &bindings
                );
                argument.0
            },
            FuzzOperation::Quote(s) => {
                SExp::Cons(
                    loc.clone(),
                    register_sexp(SExp::atom_from_string(loc.clone(), &"q".to_string())),
                    register_sexp(s.clone())
                )
            },
            FuzzOperation::If(cond,ct,cf) => make_operator(
                "if".to_string(),
                vec!(
                    cond.to_sexp(fun, bindings),
                    ct.to_sexp(fun, bindings),
                    cf.to_sexp(fun, bindings)
                )
            ),
            FuzzOperation::Multiply(a,b) => make_operator(
                "*".to_string(),
                vec!(
                    a.to_sexp(fun, bindings),
                    b.to_sexp(fun, bindings)
                )
            ),
            FuzzOperation::Sub(a,b) => make_operator(
                "-".to_string(),
                vec!(
                    a.to_sexp(fun, bindings),
                    b.to_sexp(fun, bindings)
                )
            ),
            FuzzOperation::Sha256(ents) => make_operator(
                "sha256".to_string(),
                ents.iter().map(|x| x.to_sexp(fun, bindings)).collect()
            ),
            FuzzOperation::Let(our_bindings,body) => {
                let loc = Srcloc::start(&"*rng*".to_string());
                let mut bindings_done = SExp::Nil(loc.clone());

                for b in our_bindings.iter().rev() {
                    bindings_done =
                        SExp::Cons(
                            loc.clone(),
                            register_sexp(SExp::Cons(
                                loc.clone(),
                                register_sexp(SExp::Atom(loc.clone(), b.name.clone())),
                                register_sexp(SExp::Cons(
                                    loc.clone(),
                                    register_sexp(b.expr.to_sexp(fun, bindings)),
                                    register_sexp(SExp::Nil(loc.clone()))
                                ))
                            )),
                            register_sexp(bindings_done)
                        );
                }

                let mut inner_bindings = bindings.to_vec();
                inner_bindings.push(our_bindings.clone());

                make_operator(
                    "let".to_string(),
                    vec!(
                        bindings_done,
                        body.to_sexp(
                            fun,
                            &inner_bindings
                        )
                    )
                )
            },
            FuzzOperation::Call(selection,args) => {
                let loc = Srcloc::start(&"*rng*".to_string());
                let called_fun = select_call(*selection, fun);
                let mut reified_args = Vec::new();
                for a in args.iter() {
                    reified_args.push(a.to_sexp(fun, bindings));
                }
                let args =
                    distribute_args(
                        called_fun.1.args.clone(),
                        fun,
                        bindings,
                        &reified_args,
                        true,
                        0
                    );
                SExp::Cons(
                    loc.clone(),
                    register_sexp(SExp::atom_from_string(loc.clone(), &called_fun.0)),
                    register_sexp(args.1)
                )
            }
        }
    }
}

fn make_random_call<R: Rng + ?Sized>(rng: &mut R, dialect: u32, remaining: usize) -> FuzzOperation {
    FuzzOperation::Call(
        rng.gen(),
        (0..=255).
            map(|_| random_operation(rng, dialect, remaining - 1)).
            collect()
    )
}

// FuzzOperation is potentially infinite so we'll limit the depth to something
// sensible.
fn random_operation<R: Rng + ?Sized>(rng: &mut R, dialect: u32, remaining: usize) -> FuzzOperation {
    if remaining < 2 {
        FuzzOperation::Quote(random_sexp(rng, remaining))
    } else {
        let op_bound = if dialect >= 21 { 7 } else { 6 };
        let alternative: usize = rng.gen_range(0..=op_bound);
        match alternative {
            0 => FuzzOperation::Argref(rng.gen()),
            1 => FuzzOperation::If(
                register_op(random_operation(rng, dialect, remaining - 1)),
                register_op(random_operation(rng, dialect, remaining - 1)),
                register_op(random_operation(rng, dialect, remaining - 1))
            ),
            2 => FuzzOperation::Multiply(
                register_op(random_operation(rng, dialect, remaining - 1)),
                register_op(random_operation(rng, dialect, remaining - 1))
            ),
            3 => FuzzOperation::Sub(
                register_op(random_operation(rng, dialect, remaining - 1)),
                register_op(random_operation(rng, dialect, remaining - 1))
            ),
            4 => {
                let bound: usize = rng.gen_range(0..=MAX_LIST_BOUND);
                FuzzOperation::Sha256(
                    (0..=bound).
                        map(|_| {
                            random_operation(rng, dialect, remaining - 1)
                        }).
                        collect()
                )
            },
            5 => make_random_call(rng, dialect, remaining - 1),
            6 => FuzzOperation::Quote(random_sexp(rng, remaining)),
            _ => {
                let bound: usize = rng.gen_range(1..=5);
                let new_bindings: Vec<FuzzBinding> = (1..=bound).
                    map(|_| {
                        FuzzBinding {
                            name: random_atom_name(rng, BINDING_NAME_MIN),
                            expr: random_operation(rng, dialect, remaining - 1)
                        }
                    }).
                    collect();
                FuzzOperation::Let(
                    new_bindings,
                    register_op(rng.gen())
                )
            }
        }
    }
}

impl Distribution<FuzzOperation> for Standard {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> FuzzOperation {
        random_operation(rng, 22, MAX_LIST_BOUND)
    }
}

fn min_arglist(remaining: usize) -> usize { max(remaining, MIN_ARGLIST) }

fn random_arglist_cons<R: Rng + ?Sized>(rng: &mut R, loc: &Srcloc, remaining: usize) -> SExp {
    if rng.gen() || remaining < 1 {
        SExp::Atom(loc.clone(), random_atom_name(rng, 2))
    } else {
        let left = random_arglist_cons(rng, loc, remaining - 1);
        let right = random_arglist_cons(rng, loc, remaining - 1);
        SExp::Cons(
            loc.clone(),
            Rc::new(left),
            Rc::new(right)
        )
    }
}

fn random_arglist<R: Rng + ?Sized>(rng: &mut R, remaining: usize) -> ArgListType {
    let loc = Srcloc::start("*arglist*");
    let truncated_len = (remaining % 255) as u8;
    if rng.gen() {
        ArgListType::ProperList(rng.gen_range(0..=truncated_len))
    } else {
        ArgListType::Structure(random_arglist_cons(rng, &loc, min_arglist(remaining)))
    }
}

impl Distribution<ArgListType> for Standard {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> ArgListType {
        random_arglist(rng, MAX_LIST_BOUND)
    }
}

impl ArgListType {
    pub fn random_args(&self) -> SExp {
        let loc = Srcloc::start(&"*rng*".to_string());
        match self {
            ArgListType::ProperList(n) => {
                let mut args = SExp::Nil(loc.clone());
                for _ in 0..*n {
                    let random_bytes: Vec<u8> =
                        (0..=MAX_LIST_BOUND).
                        map(|_| random()).
                        collect();
                    args = SExp::Cons(
                        args.loc(),
                        register_sexp(SExp::atom_from_vec(
                            loc.clone(),
                            &random_bytes
                        )),
                        register_sexp(args.clone())
                    );
                }
                args
            },
            ArgListType::Structure(SExp::Nil(l)) => SExp::Nil(l.clone()),
            ArgListType::Structure(SExp::Cons(l,a,b)) => {
                let aborrow: &SExp = a.borrow();
                let bborrow: &SExp = b.borrow();
                let aclone = aborrow.clone();
                let bclone = bborrow.clone();
                let arg_a = ArgListType::Structure(aclone).random_args();
                let arg_b = ArgListType::Structure(bclone).random_args();
                SExp::Cons(l.clone(), register_sexp(arg_a), register_sexp(arg_b))
            },
            ArgListType::Structure(_) => random(),
        }
    }

    fn to_sexp(&self) -> SExp {
        let loc = Srcloc::start(&"*rng*".to_string());
        match self {
            ArgListType::ProperList(n) => {
                let mut args = SExp::Nil(loc.clone());
                for i_reverse in 0..*n {
                    let i = n - i_reverse;
                    args = SExp::Cons(
                        args.loc(),
                        register_sexp(SExp::atom_from_string(
                            loc.clone(), &format!("arg_{}", i)
                        )),
                        register_sexp(args.clone())
                    );
                }
                args
            },
            ArgListType::Structure(s) => { s.clone() }
        }
    }
}

fn random_function<R: Rng + ?Sized>(rng: &mut R, dialect: u32, remaining: usize) -> FuzzFunction {
    FuzzFunction {
        inline: rng.gen(),
        number: 0,
        args: random_arglist(rng, remaining - 1),
        body: random_operation(rng, dialect, remaining - 1)
    }
}

impl Distribution<FuzzFunction> for Standard {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> FuzzFunction {
        random_function(rng, CURRENT_DIALECT, MAX_LIST_BOUND)
    }
}

impl FuzzFunction {
    fn to_sexp(&self, fun: &FuzzProgram) -> SExp {
        let fuzzloc = Srcloc::start(&"*fuzz*".to_string());
        let initial_atom =
            if self.inline {
                SExp::atom_from_string(
                    fuzzloc.clone(),
                    &"defun-inline".to_string()
                )
            } else {
                SExp::atom_from_string(
                    fuzzloc.clone(),
                    &"defun".to_string()
                )
            };
        let name_atom =
            SExp::atom_from_string(
                fuzzloc.clone(),
                &format!("fun_{}", self.number)
            );
        let args_sexp = self.args.to_sexp();
        let body_sexp = self.body.to_sexp(&self.to_program(fun), &Vec::new());
        SExp::Cons(
            fuzzloc.clone(),
            register_sexp(initial_atom),
            register_sexp(SExp::Cons(
                fuzzloc.clone(),
                register_sexp(name_atom),
                register_sexp(SExp::Cons(
                    fuzzloc.clone(),
                    register_sexp(args_sexp),
                    register_sexp(SExp::Cons(
                        fuzzloc.clone(),
                        register_sexp(body_sexp),
                        register_sexp(SExp::Nil(fuzzloc.clone()))
                    ))
                ))
            ))
        )
    }

    fn to_program(&self, parent: &FuzzProgram) -> FuzzProgram {
        FuzzProgram {
            args: self.args.clone(),
            functions: parent.functions.clone(),
            body: self.body.clone()
        }
    }
}

/*
 * Produce chialisp frontend code with an expected result
 */
fn random_program<R: Rng + ?Sized>(rng: &mut R, dialect: u32, remaining: usize) -> FuzzProgram {
    let num_funs = rng.gen_range(1..=MAX_LIST_BOUND);
    let funs: Vec<FuzzFunction> = (1..=num_funs).map(|_| random_function(rng, dialect, remaining - 1))
        .enumerate()
        .map(|(i, f) : (usize, FuzzFunction)| {
            let mut fcopy = f.clone();
            fcopy.number = i as u8;
            fcopy
        }).
        collect();
    FuzzProgram {
        args: random_arglist(rng, remaining),
        functions: funs,
        body: random_operation(rng, dialect, remaining)
    }
}

impl Distribution<FuzzProgram> for Standard {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> FuzzProgram {
        random_program(rng, CURRENT_DIALECT, MAX_LIST_BOUND)
    }
}

fn evaluate_to_numbers(prog: &FuzzProgram, args: &SExp, bindings: &[Vec<FuzzBinding>], a: &FuzzOperation, b: &FuzzOperation, steps: usize) -> Result<(BigInt, BigInt), RunFailure> {
    let a_val = interpret_program(prog, args, bindings, a, steps - 1)?;
    let b_val = interpret_program(prog, args, bindings, b, steps - 1)?;
    match (&a_val, &b_val) {
        (SExp::Integer(_, a), SExp::Integer(_, b)) => {
            Ok((a.clone(), b.clone()))
        },
        (SExp::Cons(l,_,_), _) => {
            Err(RunFailure::RunErr(l.clone(), format!("*: expected atom got {}", a_val.to_string())))
        },
        (_, SExp::Cons(l,_,_)) => {
            Err(RunFailure::RunErr(l.clone(), format!("*: expected atom got {}", b_val.to_string())))
        },
        (a, b) => {
            let num_a = a.get_number().map_err(|e| {
                RunFailure::RunErr(a.loc(), e.1)
            })?;
            let num_b = b.get_number().map_err(|e| {
                RunFailure::RunErr(b.loc(), e.1)
            })?;
            Ok((num_a, num_b))
        }
    }
}

fn byte_vec_of_sexp(val: &SExp) -> Result<Vec<u8>, RunFailure> {
    match val {
        SExp::Nil(_) => Ok(Vec::new()),
        SExp::Atom(_,a) => Ok(a.clone()),
        SExp::QuotedString(_,_,s) => Ok(s.clone()),
        SExp::Integer(_,i) => Ok(bigint_to_bytes_clvm(i).raw()),
        _ => Err(RunFailure::RunErr(val.loc(), format!("attempt to convert {} to bytes", val.to_string())))
    }
}

fn choose_path(path: Number, args: Rc<SExp>) -> Result<Rc<SExp>, RunFailure> {
    if path == bi_one() {
        Ok(args)
    } else {
        match args.borrow() {
            SExp::Cons(_,a,b) => {
                let odd = bi_one() & path.clone();
                if odd != bi_zero() {
                    choose_path(path >> 1, b.clone())
                } else {
                    choose_path(path >> 1, a.clone())
                }
            },
            _ => Err(RunFailure::RunErr(args.loc(), "path into atom".to_string()))
        }
    }
}

fn interpret_program(prog: &FuzzProgram, args: &SExp, bindings: &[Vec<FuzzBinding>], expr: &FuzzOperation, steps: usize) -> Result<SExp, RunFailure> {
    if steps < 1 {
        return Err(RunFailure::RunErr(args.loc(), "too many steps taken".to_string()));
    }
    let loc = Srcloc::start(&"*interp*".to_string());
    match &expr {
        FuzzOperation::Argref(n) => {
            let (argname, run_expression) =
                select_argument(*n as usize, prog, bindings);
            if let Some(to_run) = run_expression {
                // Run binding code selected.
                interpret_program(
                    prog,
                    args,
                    bindings,
                    &to_run,
                    steps - 1
                )
            } else {
                // Select argument from env.
                let argpath = create_name_lookup_(
                    args.loc(),
                    &argname.to_string().as_bytes(),
                    register_sexp(prog.args.to_sexp()),
                    register_sexp(prog.args.to_sexp())
                ).map_err(|e| RunFailure::RunErr(e.0.clone(), e.1.clone()))?;
                let argval = choose_path(argpath.to_bigint().unwrap(), register_sexp(args.clone()))?;
                let argval_borrow: &SExp = argval.borrow();
                interpret_program(
                    prog,
                    args,
                    bindings,
                    &FuzzOperation::Quote(argval_borrow.clone()),
                    steps - 1
                )
            }
        },
        FuzzOperation::Quote(exp) => Ok(exp.clone()),
        FuzzOperation::If(cond,iftrue,iffalse) => {
            let borrowed_cond: &FuzzOperation = cond.borrow();
            interpret_program(prog, args, bindings, borrowed_cond, steps - 1).map(|cond_res| {
                truthy(register_sexp(cond_res))
            }).and_then(|cond_res| {
                if cond_res {
                    let borrowed_iftrue: &FuzzOperation = iftrue.borrow();
                    interpret_program(prog, args, bindings, borrowed_iftrue, steps - 1)
                } else {
                    let borrowed_iffalse: &FuzzOperation = iffalse.borrow();
                    interpret_program(prog, args, bindings, borrowed_iffalse, steps - 1)
                }
            })
        },
        FuzzOperation::Multiply(a,b) => {
            let (a_val, b_val) = evaluate_to_numbers(prog, args, bindings, a.borrow(), b.borrow(), steps - 1)?;
            Ok(SExp::Integer(loc, a_val * b_val))
        },
        FuzzOperation::Sub(a,b) => {
            let (a_val, b_val) = evaluate_to_numbers(prog, args, bindings, a.borrow(), b.borrow(), steps - 1)?;
            Ok(SExp::Integer(loc, a_val - b_val))
        },
        FuzzOperation::Sha256(lst) => {
            let loc = Srcloc::start(&"*sha256*".to_string());
            let mut bytes_stream = Stream::new(None);
            for elt in lst.iter() {
                let output = interpret_program(prog, args, bindings, &elt, steps - 1)?;
                let output_bytes = byte_vec_of_sexp(&output)?;
                bytes_stream.write(
                    Bytes::new(Some(BytesFromType::Raw(output_bytes)))
                );
            }
            Ok(SExp::Atom(loc, sha256(bytes_stream.get_value()).data().clone()))
        },
        FuzzOperation::Let(new_bindings,body) => {
            let mut total_bindings = bindings.to_vec();
            total_bindings.push(new_bindings.clone());
            interpret_program(prog, args, &total_bindings, body.borrow(), steps - 1)
        },
        FuzzOperation::Call(fun,call_args) => {
            let called_fun = select_call(*fun, prog);
            let mut reified_args = Vec::new();

            // Interpret all arguments.
            for a in call_args.iter() {
                reified_args.push(interpret_program(prog, args, bindings, a, steps - 1)?);
            }

            // Use reified arguments since we're assuming they're sexp.
            let distributed_args =
                distribute_args(
                    called_fun.1.args.clone(),
                    prog,
                    bindings,
                    &reified_args,
                    true,
                    0
                );
            println!("call {} with args: {} (parent {} funs)", called_fun.1.to_sexp(prog).to_string(), distributed_args.1.to_string(), prog.functions.len());
            interpret_program(
                &called_fun.1.to_program(prog),
                &distributed_args.1,
                &Vec::new(),
                &called_fun.1.body.clone(),
                steps - 1
            )
        }
    }
}

impl FuzzProgram {
    pub fn to_sexp(&self) -> SExp {
        let mut body_vec = Vec::new();
        body_vec.push(self.args.to_sexp());
        for f in &self.functions {
            body_vec.push(f.to_sexp(self))
        }
        body_vec.push(self.body.to_sexp(self, &Vec::new()));
        make_operator("mod".to_string(), body_vec)
    }

    pub fn random_args(&self) -> SExp {
        let srcloc = Srcloc::start(&"*args*".to_string());
        random_args(srcloc, self.args.clone())
    }

    pub fn interpret(&self, args: SExp) -> Result<SExp, RunFailure> {
        interpret_program(self, &args, &Vec::new(), &self.body, MAX_STEPS)
    }
}

pub fn random_old_program<R: Rng + ?Sized>(rng: &mut R, remaining: usize) -> FuzzOldProgram {
    FuzzOldProgram {
        program: random_program(rng, 0, remaining)
    }
}

impl Distribution<FuzzOldProgram> for Standard {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> FuzzOldProgram {
        random_old_program(rng, MAX_LIST_BOUND)
    }
}

pub fn make_random_u64_seed() -> u64 {
    let mut rng = ChaCha8Rng::from_entropy();
    let random_seed = random_atom_name(&mut rng, 10);
    let random_seed_as_bigint =
        number_from_u8(&random_seed) & 0xffffffffffff_u64.to_bigint().unwrap();
    random_seed_as_bigint.to_u64().unwrap()
}
