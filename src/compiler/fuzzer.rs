use num_bigint::{BigInt, ToBigInt};

use rand::distributions::Standard;
use rand::prelude::Distribution;
use rand::Rng;
use rand::random;
use std::borrow::Borrow;
use std::rc::Rc;

use crate::compiler::clvm::truthy;
use crate::compiler::sexp::{SExp, enlist, random_sexp};
use crate::compiler::srcloc::Srcloc;
use crate::compiler::runtypes::RunFailure;
use crate::classic::clvm::__type_compatibility__::{
    Bytes,
    BytesFromType,
    Stream,
    sha256
};
use crate::classic::clvm::casts::{
    TConvertOption,
    bigint_to_bytes
};

const MAX_LIST_BOUND: usize = 15;

// We don't actually need all operators here, just a good selection with
// semantics that are distinguishable.
#[derive(Debug, Clone)]
pub enum FuzzOperation {
    Argref(u8),
    Quote(SExp),
    If(Rc<FuzzOperation>,Rc<FuzzOperation>,Rc<FuzzOperation>),
    Multiply(Rc<FuzzOperation>,Rc<FuzzOperation>),
    Sub(Rc<FuzzOperation>,Rc<FuzzOperation>),
    Sha256(Vec<FuzzOperation>),
    Let(Vec<FuzzOperation>,Rc<FuzzOperation>),
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

#[derive(Debug)]
pub struct FuzzProgram {
    pub args: ArgListType,
    pub functions: Vec<FuzzFunction>,
    pub body: FuzzOperation,
}

fn atom_list(sexp: &SExp) -> Vec<String> {
    match sexp {
        SExp::Nil(_) => vec!(),
        SExp::Atom(_,_) => vec!(sexp.to_string()),
        SExp::QuotedString(_,_,_) => vec!(sexp.to_string()),
        SExp::Integer(_,_) => vec!(sexp.to_string()),
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

fn select_argument(num_: u8, fun: &FuzzProgram, bindings: &Vec<Vec<FuzzOperation>>) -> (String, FuzzOperation) {
    let mut num = num_;
    let args = atom_list(&fun.args.to_sexp());

    loop {
        if num < args.len() as u8 {
            return (args[num as usize].clone(), FuzzOperation::Argref(num));
        }

        num -= args.len() as u8;
        let mut binding_count = 0;
        for b_list in bindings {
            if num < b_list.len() as u8 {
                return (
                    format!("binding_{}", binding_count + num),
                    b_list[num as usize].clone()
                );
            }

            num -= b_list.len() as u8;
            binding_count += b_list.len() as u8;
        }
    }
}

fn select_call(num: u8, prog: &FuzzProgram) -> (String, FuzzFunction) {
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
            Rc::new(args[i].clone()),
            Rc::new(result)
        );
    }

    SExp::Cons(
        loc.clone(),
        Rc::new(SExp::atom_from_string(loc.clone(), &op)),
        Rc::new(result)
    )
}

fn distribute_args(a: ArgListType, fun: &FuzzProgram, bindings: &Vec<Vec<FuzzOperation>>, arginputs: &Vec<SExp>, spine: bool, argn: u8) -> (u8, SExp) {
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
                    Rc::new(arginputs[argn as usize].clone()),
                    Rc::new(rest_result.1)
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
                    SExp::Cons(l.clone(), Rc::new(first_res.1), Rc::new(rest_res.1))
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
            (
                argn + 1_u8,
                arginputs[argn as usize].clone()
            )
        }
    }
}

fn random_args(loc: Srcloc, a: ArgListType) -> SExp {
    match a {
        ArgListType::ProperList(0) => SExp::Nil(loc.clone()),
        ArgListType::ProperList(n) => {
            let loc = Srcloc::start("*rng*");
            enlist(loc, (0..n).map(|_| Rc::new(random())).collect())
        },
        ArgListType::Structure(SExp::Nil(l)) => SExp::Nil(l.clone()),
        ArgListType::Structure(SExp::Cons(_,a,b)) => {
            let borrowed_a: &SExp = a.borrow();
            let borrowed_b: &SExp = b.borrow();
            SExp::Cons(
                loc.clone(),
                Rc::new(random_args(loc.clone(), ArgListType::Structure(borrowed_a.clone()))),
                Rc::new(random_args(loc.clone(), ArgListType::Structure(borrowed_b.clone())))
            )
        },
        ArgListType::Structure(_) => {
            let random_64: u64 = random();
            SExp::Integer(loc.clone(), random_64.to_bigint().unwrap())
        }
    }
}

impl FuzzOperation {
    pub fn to_sexp(&self, fun: &FuzzProgram, bindings: &Vec<Vec<FuzzOperation>>) -> SExp {
        let loc = Srcloc::start(&"*rng*".to_string());
        match self {
            FuzzOperation::Argref(argument_num) => {
                let argument = select_argument(
                    *argument_num, fun, bindings
                );
                SExp::atom_from_string(loc.clone(), &argument.0)
            },
            FuzzOperation::Quote(s) => {
                SExp::Cons(
                    loc.clone(),
                    Rc::new(SExp::atom_from_string(loc.clone(), &"q".to_string())),
                    Rc::new(s.clone())
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

                for i_reverse in 0..our_bindings.len() {
                    let i = our_bindings.len() - i_reverse - 1;
                    let binding_name =
                        format!("binding_{}", i);
                    let b = &our_bindings[i];

                    bindings_done =
                        SExp::Cons(
                            loc.clone(),
                            Rc::new(SExp::Cons(
                                loc.clone(),
                                Rc::new(SExp::atom_from_string(loc.clone(), &binding_name)),
                                Rc::new(SExp::Cons(
                                    loc.clone(),
                                    Rc::new(b.to_sexp(fun, bindings)),
                                    Rc::new(SExp::Nil(loc.clone()))
                                ))
                            )),
                            Rc::new(bindings_done)
                        );
                }

                let mut inner_bindings = bindings.clone();
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
                    Rc::new(SExp::atom_from_string(loc.clone(), &called_fun.0)),
                    Rc::new(args.1)
                )
            }
        }
    }
}

fn make_random_call<R: Rng + ?Sized>(rng: &mut R, remaining: usize) -> FuzzOperation {
    FuzzOperation::Call(
        rng.gen(),
        (0..=255).
            map(|_| random_operation(rng, remaining - 1)).
            collect()
    )
}

// FuzzOperation is potentially infinite so we'll limit the depth to something
// sensible.
fn random_operation<R: Rng + ?Sized>(rng: &mut R, remaining: usize) -> FuzzOperation {
    if remaining < 2 {
        FuzzOperation::Quote(random_sexp(rng, remaining))
    } else {
        let alternative: usize = rng.gen_range(0..=7);
        match alternative {
            0 => FuzzOperation::Argref(rng.gen()),
            1 => FuzzOperation::If(
                Rc::new(random_operation(rng, remaining - 1)),
                Rc::new(random_operation(rng, remaining - 1)),
                Rc::new(random_operation(rng, remaining - 1))
            ),
            2 => FuzzOperation::Multiply(
                Rc::new(random_operation(rng, remaining - 1)),
                Rc::new(random_operation(rng, remaining - 1))
            ),
            3 => FuzzOperation::Sub(
                Rc::new(random_operation(rng, remaining - 1)),
                Rc::new(random_operation(rng, remaining - 1))
            ),
            4 => {
                let bound: usize = rng.gen_range(0..=MAX_LIST_BOUND);
                FuzzOperation::Sha256(
                    (0..=bound).
                        map(|_| {
                            random_operation(rng, remaining - 1)
                        }).
                        collect()
                )
            },
            5 => {
                let bound: usize = rng.gen_range(0..=5);
                FuzzOperation::Let(
                    (0..=bound).
                        map(|_| {
                            random_operation(rng, remaining - 1)
                        }).
                        collect(),
                    Rc::new(rng.gen())
                )
            },
            6 => make_random_call(rng, remaining - 1),
            _ => FuzzOperation::Quote(random_sexp(rng, remaining))
        }
    }
}

impl Distribution<FuzzOperation> for Standard {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> FuzzOperation {
        random_operation(rng, 0)
    }
}

impl Distribution<ArgListType> for Standard {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> ArgListType {
        match rng.gen_range(0..=1) {
            0 => ArgListType::ProperList(rng.gen_range(0..=5)),
            _ => ArgListType::Structure(random())
        }
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
                        Rc::new(SExp::atom_from_vec(
                            loc.clone(),
                            &random_bytes
                        )),
                        Rc::new(args.clone())
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
                SExp::Cons(l.clone(), Rc::new(arg_a), Rc::new(arg_b))
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
                        Rc::new(SExp::atom_from_string(
                            loc.clone(), &format!("arg_{}", i)
                        )),
                        Rc::new(args.clone())
                    );
                }
                args
            },
            ArgListType::Structure(s) => { s.clone() }
        }
    }
}

impl Distribution<FuzzFunction> for Standard {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> FuzzFunction {
        FuzzFunction {
            inline: rng.sample(self),
            number: 0,
            args: rng.sample(self),
            body: rng.sample(self)
        }
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
            Rc::new(initial_atom),
            Rc::new(SExp::Cons(
                fuzzloc.clone(),
                Rc::new(name_atom),
                Rc::new(SExp::Cons(
                    fuzzloc.clone(),
                    Rc::new(args_sexp),
                    Rc::new(SExp::Cons(
                        fuzzloc.clone(),
                        Rc::new(body_sexp),
                        Rc::new(SExp::Nil(fuzzloc.clone()))
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
impl Distribution<FuzzProgram> for Standard {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> FuzzProgram {
        let funs = (1..=MAX_LIST_BOUND).map(|_| rng.gen())
            .enumerate()
            .map(|(i, f) : (usize, FuzzFunction)| {
                let mut fcopy = f.clone();
                fcopy.number = i as u8;
                fcopy
            }).
            collect();
        FuzzProgram {
            args: random(),
            functions: funs,
            body: random_operation(rng, 0)
        }
    }
}

fn evaluate_to_numbers(prog: &FuzzProgram, args: &SExp, bindings: &Vec<Vec<FuzzOperation>>, a: &FuzzOperation, b: &FuzzOperation) -> Result<(BigInt, BigInt), RunFailure> {
    let a_val = interpret_program(prog, args, bindings, a)?;
    let b_val = interpret_program(prog, args, bindings, b)?;
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
        SExp::Integer(_,i) => bigint_to_bytes(i, Some(TConvertOption { signed: true })).map_err(|e| RunFailure::RunErr(val.loc(), e)).map(|x| x.data().clone()),
        _ => Err(RunFailure::RunErr(val.loc(), format!("attempt to convert {} to bytes", val.to_string())))
    }
}

fn interpret_program(prog: &FuzzProgram, args: &SExp, bindings: &Vec<Vec<FuzzOperation>>, expr: &FuzzOperation) -> Result<SExp, RunFailure> {
    let loc = Srcloc::start(&"*interp*".to_string());
    eprintln!("interpret_program {} with args {} and expr {}", prog.to_sexp(), args, expr.to_sexp(prog, bindings));

    match &expr {
        FuzzOperation::Argref(n) => {
            let (_, argval) = select_argument(*n, prog, bindings);
            interpret_program(
                prog,
                args,
                bindings,
                &argval
            )
        },
        FuzzOperation::Quote(exp) => Ok(exp.clone()),
        FuzzOperation::If(cond,iftrue,iffalse) => {
            let borrowed_cond: &FuzzOperation = cond.borrow();
            interpret_program(prog, args, bindings, borrowed_cond).map(|cond_res| {
                truthy(Rc::new(cond_res))
            }).and_then(|cond_res| {
                if cond_res {
                    let borrowed_iftrue: &FuzzOperation = iftrue.borrow();
                    interpret_program(prog, args, bindings, borrowed_iftrue)
                } else {
                    let borrowed_iffalse: &FuzzOperation = iffalse.borrow();
                    interpret_program(prog, args, bindings, borrowed_iffalse)
                }
            })
        },
        FuzzOperation::Multiply(a,b) => {
            let (a_val, b_val) = evaluate_to_numbers(prog, args, bindings, a.borrow(), b.borrow())?;
            Ok(SExp::Integer(loc, a_val * b_val))
        },
        FuzzOperation::Sub(a,b) => {
            let (a_val, b_val) = evaluate_to_numbers(prog, args, bindings, a.borrow(), b.borrow())?;
            Ok(SExp::Integer(loc, a_val - b_val))
        },
        FuzzOperation::Sha256(lst) => {
            let loc = Srcloc::start(&"*sha256*".to_string());
            let mut bytes_stream = Stream::new(None);
            for elt in lst.iter() {
                let output = interpret_program(prog, args, bindings, &elt)?;
                let output_bytes = byte_vec_of_sexp(&output)?;
                bytes_stream.write(
                    Bytes::new(Some(BytesFromType::Raw(output_bytes)))
                );
            }
            Ok(SExp::Atom(loc, sha256(bytes_stream.get_value()).data().clone()))
        },
        FuzzOperation::Let(new_bindings,body) => {
            let mut total_bindings = bindings.clone();
            total_bindings.push(new_bindings.clone());
            interpret_program(prog, args, &total_bindings, body.borrow())
        },
        FuzzOperation::Call(fun,call_args) => {
            let called_fun = select_call(*fun, prog);
            let mut reified_args = Vec::new();

            // Interpret all arguments.
            for a in call_args.iter() {
                reified_args.push(interpret_program(prog, args, bindings, a)?);
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
            println!("call {} with args: {}", called_fun.1.to_sexp(prog).to_string(), distributed_args.1.to_string());
            interpret_program(
                &called_fun.1.to_program(prog),
                &distributed_args.1,
                &Vec::new(),
                &called_fun.1.body.clone()
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
        interpret_program(self, &args, &Vec::new(), &self.body)
    }
}
