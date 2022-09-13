use std::borrow::Borrow;
use std::cell::{Ref, RefCell};
use std::collections::HashMap;
use std::mem::swap;
use std::rc::Rc;
use std::sync::atomic::AtomicI32;
use std::sync::atomic::Ordering;

use num_bigint::ToBigInt;

use clvm_rs::allocator::{Allocator, NodePtr, SExp};
use clvm_rs::cost::Cost;
use clvm_rs::reduction::{EvalErr, Reduction, Response};

use crate::classic::clvm::__type_compatibility__::{bi_one, bi_zero};
use crate::classic::clvm::sexp::{
    atom, enlist, equal_to, first, fold_m, map_m, non_nil, proper_list,
};
use crate::classic::clvm_tools::binutils::disassemble;
use crate::classic::clvm_tools::node_path::NodePath;
use crate::classic::clvm_tools::pattern_match::match_sexp;
use crate::classic::clvm_tools::sha256tree::sha256tree;
use crate::classic::clvm_tools::stages::assemble;
use crate::classic::clvm_tools::stages::stage_0::TRunProgram;
use crate::classic::clvm_tools::stages::stage_2::helpers::quote;

use crate::util::{number_from_u8, u8_from_number};

use crate::compiler::sexp::decode_string;

#[derive(Clone)]
pub struct DoOptProg {}

lazy_static! {
    pub static ref DEPTH_COUNT: AtomicI32 = AtomicI32::new(0);
}

pub fn depth_count() -> i32 {
    DEPTH_COUNT.fetch_add(0, Ordering::SeqCst)
}

pub fn enter() {
    DEPTH_COUNT.fetch_add(1, Ordering::SeqCst);
}

pub fn leave() {
    DEPTH_COUNT.fetch_add(-1, Ordering::SeqCst);
}

pub fn indent() -> String {
    let dc = depth_count() as usize;
    let mut v = Vec::with_capacity(dc);
    for _ in 0..dc {
        v.push(b' ');
    }
    decode_string(&v)
}

const DEBUG_OPTIMIZATIONS: bool = false;
const DIAG_OPTIMIZATIONS: bool = false;

pub fn seems_constant_tail(allocator: &mut Allocator, sexp_: NodePtr) -> bool {
    let mut sexp = sexp_;

    loop {
        match allocator.sexp(sexp) {
            SExp::Pair(l, r) => {
                if !seems_constant(allocator, l) {
                    return false;
                }

                sexp = r;
            }
            SExp::Atom(_) => {
                return sexp == allocator.null();
            }
        }
    }
}

pub fn seems_constant(allocator: &mut Allocator, sexp: NodePtr) -> bool {
    match allocator.sexp(sexp) {
        SExp::Atom(_b) => {
            return sexp == allocator.null();
        }
        SExp::Pair(operator, r) => {
            match allocator.sexp(operator) {
                SExp::Atom(b) => {
                    let atom = allocator.buf(&b);
                    if atom.len() == 1 && atom[0] == 1 {
                        return true;
                    } else if atom.len() == 1 && atom[0] == 8 {
                        return false;
                    }
                }
                SExp::Pair(_, _) => {
                    if !seems_constant(allocator, operator) {
                        return false;
                    }
                }
            }

            if !seems_constant_tail(allocator, r) {
                return false;
            }
        }
    }
    true
}

pub fn constant_optimizer(
    allocator: &mut Allocator,
    _memo: Rc<RefCell<HashMap<String, NodePtr>>>,
    r: NodePtr,
    _max_cost: Cost,
    runner: Rc<dyn TRunProgram>,
) -> Result<NodePtr, EvalErr> {
    /*
     * If the expression does not depend upon @ anywhere,
     * it's a constant. So we can simply evaluate it and
     * return the quoted result.
     */
    if let SExp::Pair(first, _) = allocator.sexp(r) {
        if let SExp::Atom(b) = allocator.sexp(first) {
            let buf = allocator.buf(&b);
            if buf.len() == 1 && buf[0] == 1 {
                // Short circuit already quoted expression.
                return Ok(r);
            }
        }
    }

    let sc_r = seems_constant(allocator, r);
    let nn_r = non_nil(allocator, r);
    if DIAG_OPTIMIZATIONS {
        println!(
            "COPT {} SC_R {} NN_R {}",
            disassemble(allocator, r),
            sc_r,
            nn_r
        );
    }
    if sc_r && nn_r {
        return m! {
            res <- runner.run_program(
                allocator,
                r,
                allocator.null(),
                None
            );
            let r1 = res.1;
            let _ = if DIAG_OPTIMIZATIONS {
                println!(
                    "CONSTANT_OPTIMIZER {} TO {}\n",
                    disassemble(allocator, r),
                    disassemble(allocator, r1)
                );
            };
            quoted <- quote(allocator, r1);
            Ok(quoted)
        };
    }

    Ok(r)
}

pub fn is_args_call(allocator: &mut Allocator, r: NodePtr) -> bool {
    if let SExp::Atom(b) = allocator.sexp(r) {
        let buf = allocator.buf(&b);
        buf.len() == 1 && buf[0] == 1
    } else {
        false
    }
}

pub fn cons_q_a_optimizer_pattern(allocator: &mut Allocator) -> NodePtr {
    assemble(allocator, "(a (q . (: . sexp)) (: . args))").unwrap()
}

pub fn cons_q_a_optimizer(
    allocator: &mut Allocator,
    _memo: Rc<RefCell<HashMap<String, NodePtr>>>,
    r: NodePtr,
    _eval_f: Rc<dyn TRunProgram>,
) -> Result<NodePtr, EvalErr> {
    let cons_q_a_optimizer_pattern = cons_q_a_optimizer_pattern(allocator);

    /*
     * This applies the transform
     * (a (q . SEXP) @) => SEXP
     */

    let matched = match_sexp(allocator, cons_q_a_optimizer_pattern, r, HashMap::new());

    return match (
        matched.as_ref().and_then(|t1| t1.get("args").copied()),
        matched.as_ref().and_then(|t1| t1.get("sexp").copied()),
    ) {
        (Some(args), Some(sexp)) => {
            if is_args_call(allocator, args) {
                Ok(sexp)
            } else {
                Ok(r)
            }
        }
        _ => Ok(r),
    };
}

fn cons_pattern(allocator: &mut Allocator) -> NodePtr {
    assemble(allocator, "(c (: . first) (: . rest)))").unwrap()
}

fn cons_f(allocator: &mut Allocator, args: NodePtr) -> Result<NodePtr, EvalErr> {
    m! {
        let cons_pattern = cons_pattern(allocator);
        if let Some(first) = match_sexp(allocator, cons_pattern, args, HashMap::new()).and_then(|t| t.get("first").copied()) {
            Ok(first)
        } else {
            m! {
                first_atom <- allocator.new_atom(&[5]);
                tail <- allocator.new_pair(args, allocator.null());
                allocator.new_pair(first_atom, tail)
            }
        }
    }
}

fn cons_r(allocator: &mut Allocator, args: NodePtr) -> Result<NodePtr, EvalErr> {
    m! {
        let cons_pattern = cons_pattern(allocator);
        if let Some(rest) = match_sexp(allocator, cons_pattern, args, HashMap::new()).and_then(|t| t.get("rest").copied()) {
            Ok(rest)
        } else {
            m! {
                rest_atom <- allocator.new_atom(&[6]);
                tail <- allocator.new_pair(args, allocator.null());
                allocator.new_pair(rest_atom, tail)
            }
        }
    }
}

fn path_from_args(
    allocator: &mut Allocator,
    sexp: NodePtr,
    new_args: NodePtr,
) -> Result<NodePtr, EvalErr> {
    match allocator.sexp(sexp) {
        SExp::Atom(v_buf) => {
            let v = number_from_u8(allocator.buf(&v_buf));
            if v <= bi_one() {
                Ok(new_args)
            } else {
                let sexp = allocator.new_atom(&u8_from_number(v.clone() >> 1).to_vec())?;
                if (v & 1_u32.to_bigint().unwrap()) != bi_zero() {
                    let cons_r_res = cons_r(allocator, new_args)?;
                    path_from_args(allocator, sexp, cons_r_res)
                } else {
                    let cons_f_res = cons_f(allocator, new_args)?;
                    path_from_args(allocator, sexp, cons_f_res)
                }
            }
        }
        _ => Ok(new_args),
    }
}

pub fn sub_args(
    allocator: &mut Allocator,
    sexp: NodePtr,
    new_args: NodePtr,
) -> Result<NodePtr, EvalErr> {
    match allocator.sexp(sexp) {
        SExp::Atom(_) => path_from_args(allocator, sexp, new_args),
        SExp::Pair(first_pre, rest) => {
            let first;

            match allocator.sexp(first_pre) {
                SExp::Pair(_, _) => {
                    first = sub_args(allocator, first_pre, new_args)?;
                }
                SExp::Atom(b) => {
                    let atom = allocator.buf(&b);
                    if atom.len() == 1 && atom[0] == 1 {
                        return Ok(sexp);
                    } else {
                        first = first_pre;
                    }
                }
            }

            match proper_list(allocator, rest, true) {
                Some(tail_args) => m! {
                    res <- map_m(
                        allocator,
                        &mut tail_args.iter(),
                        &|allocator, elt| {
                            sub_args(allocator, *elt, new_args)
                        }
                    );
                    tail_list <- enlist(allocator, &res);
                    allocator.new_pair(first, tail_list)
                },
                None => path_from_args(allocator, sexp, new_args),
            }
        }
    }
}

fn var_change_optimizer_cons_eval_pattern(allocator: &mut Allocator) -> NodePtr {
    assemble(allocator, "(a (q . (: . sexp)) (: . args))").unwrap()
}

pub fn var_change_optimizer_cons_eval(
    allocator: &mut Allocator,
    memo: Rc<RefCell<HashMap<String, NodePtr>>>,
    r: NodePtr,
    eval_f: Rc<dyn TRunProgram>,
) -> Result<NodePtr, EvalErr> {
    /*
     * This applies the transform
     * (a (q . (op SEXP1...)) (ARGS)) => (q . RET_VAL) where ARGS != @
     * via
     * (op (a SEXP1 (ARGS)) ...) (ARGS)) and then "children_optimizer" of this.
     * In some cases, this can result in a constant in some of the children.
     *
     * If we end up needing to push the "change of variables" to only one child, keep
     * the optimization. Otherwise discard it.
     */

    let pattern = var_change_optimizer_cons_eval_pattern(allocator);
    match match_sexp(allocator, pattern, r, HashMap::new()).as_ref() {
        None => Ok(r),
        Some(t1) => {
            let original_args = t1
                .get("args")
                .ok_or(EvalErr(r, "bad pattern match on args".to_string()))?;

            if DIAG_OPTIMIZATIONS {
                println!(
                    "XXX ORIGINAL_ARGS {}\n",
                    disassemble(allocator, *original_args)
                );
            };
            let original_call = t1
                .get("sexp")
                .ok_or(EvalErr(r, "bad pattern match on sexp".to_string()))?;

            if DIAG_OPTIMIZATIONS {
                println!(
                    "XXX ORIGINAL_CALL {}\n",
                    disassemble(allocator, *original_call)
                );
            };

            let new_eval_sexp_args = sub_args(allocator, *original_call, *original_args)?;

            if DIAG_OPTIMIZATIONS {
                println!(
                    "XXX new_eval_sexp_args {} ORIG {}\n",
                    disassemble(allocator, new_eval_sexp_args),
                    disassemble(allocator, *original_args)
                );
            };

            // Do not iterate into a quoted value as if it were a list
            if seems_constant(allocator, new_eval_sexp_args) {
                if DIAG_OPTIMIZATIONS {
                    println!("XXX seems_constant\n");
                }
                optimize_sexp_(allocator, memo, new_eval_sexp_args, eval_f)
            } else {
                if DIAG_OPTIMIZATIONS {
                    println!("XXX does not seems_constant\n");
                };

                let new_operands = proper_list(allocator, new_eval_sexp_args, true).ok_or(
                    EvalErr(new_eval_sexp_args, "Must be a proper list".to_string()),
                )?;

                let mut opt_operands = Vec::new();
                for item in new_operands.iter() {
                    opt_operands.push(optimize_sexp_(
                        allocator,
                        memo.clone(),
                        *item,
                        eval_f.clone(),
                    )?);
                }

                let non_constant_count = fold_m(
                    allocator,
                    &|allocator, acc, val| {
                        if DIAG_OPTIMIZATIONS {
                            println!("XXX opt_operands {} {}\n", acc, disassemble(allocator, val));
                        }
                        let increment = match allocator.sexp(val) {
                            SExp::Pair(val_first, _) => match allocator.sexp(val_first) {
                                SExp::Atom(b) => {
                                    let vf_buf = allocator.buf(&b);
                                    if vf_buf.len() != 1 || vf_buf[0] != 1 {
                                        1
                                    } else {
                                        0
                                    }
                                }
                                _ => 0,
                            },
                            _ => 0,
                        };

                        Ok(acc + increment)
                    },
                    0,
                    &mut opt_operands.iter().map(|x| *x),
                )?;

                if DIAG_OPTIMIZATIONS {
                    println!("XXX non_constant_count {}\n", non_constant_count);
                };

                if non_constant_count < 1 {
                    enlist(allocator, &opt_operands)
                } else {
                    Ok(r)
                }
            }
        }
    }
}

pub fn children_optimizer(
    allocator: &mut Allocator,
    memo: Rc<RefCell<HashMap<String, NodePtr>>>,
    r: NodePtr,
    eval_f: Rc<dyn TRunProgram>,
) -> Result<NodePtr, EvalErr> {
    // Recursively apply optimizations to all non-quoted child nodes.
    let mut different = false;
    match proper_list(allocator, r, true) {
        None => Ok(r),
        Some(list) => {
            if list.is_empty() {
                return Ok(r);
            }
            if let SExp::Atom(op_buf) = allocator.sexp(list[0]) {
                if allocator.buf(&op_buf).to_vec() == vec![1] {
                    return Ok(r);
                }
            }

            let mut optimized = Vec::new();
            for item in list.iter() {
                let res = optimize_sexp_(allocator, memo.clone(), *item, eval_f.clone())?;
                if !equal_to(allocator, *item, res) {
                    different = true;
                }
                optimized.push(res);
            }

            if different {
                enlist(allocator, &optimized)
            } else {
                // If we didn't produce any different children, skip producing
                // a new list and return r.  Take advantage of using a consistent
                // allocator to help the cache.
                Ok(r)
            }
        }
    }
}

fn cons_optimizer_pattern_first(allocator: &mut Allocator) -> NodePtr {
    assemble(allocator, "(f (c (: . first) (: . rest)))").unwrap()
}

fn cons_optimizer_pattern_rest(allocator: &mut Allocator) -> NodePtr {
    assemble(allocator, "(r (c (: . first) (: . rest)))").unwrap()
}

fn cons_optimizer(
    allocator: &mut Allocator,
    _memo: Rc<RefCell<HashMap<String, NodePtr>>>,
    r: NodePtr,
    _eval_f: Rc<dyn TRunProgram>,
) -> Result<NodePtr, EvalErr> {
    /*
     * This applies the transform
     *  (f (c A B)) => A
     *  and
     *  (r (c A B)) => B
     */
    let cons_optimizer_pattern_first = cons_optimizer_pattern_first(allocator);
    let cons_optimizer_pattern_rest = cons_optimizer_pattern_rest(allocator);

    m! {
        let t1 = match_sexp(
            allocator, cons_optimizer_pattern_first, r, HashMap::new()
        );
        match t1.and_then(|t| t.get("first").copied()) {
            Some(first) => Ok(first),
            _ => {
                m! {
                    let t2 = match_sexp(
                        allocator, cons_optimizer_pattern_rest, r, HashMap::new()
                    );
                    match t2.and_then(|t| t.get("rest").copied()) {
                        Some(rest) => Ok(rest),
                        _ => Ok(r)
                    }
                }
            }
        }
    }
}

fn first_atom_pattern(allocator: &mut Allocator) -> NodePtr {
    assemble(allocator, "(f ($ . atom))").unwrap()
}

fn rest_atom_pattern(allocator: &mut Allocator) -> NodePtr {
    assemble(allocator, "(r ($ . atom))").unwrap()
}

fn path_optimizer(
    allocator: &mut Allocator,
    _memo: Rc<RefCell<HashMap<String, NodePtr>>>,
    r: NodePtr,
    _eval_f: Rc<dyn TRunProgram>,
) -> Result<NodePtr, EvalErr> {
    let first_atom_pattern = first_atom_pattern(allocator);
    let rest_atom_pattern = rest_atom_pattern(allocator);

    /*
     * This applies the transform
     *   (f N) => A
     * and
     *   (r N) => B
     */

    let first_match = match_sexp(allocator, first_atom_pattern, r, HashMap::new());
    let rest_match = match_sexp(allocator, rest_atom_pattern, r, HashMap::new());

    return m! {
        match (first_match, rest_match) {
            (Some(first), _) => {
                match first.
                    get("atom").
                    and_then(|a| atom(allocator, *a).ok()).
                    map(|atom| number_from_u8(allocator.buf(&atom)))
                {
                    Some(atom) => {
                        let node =
                            NodePath::new(Some(atom)).
                            add(NodePath::new(None).first());
                        allocator.new_atom(node.as_path().data())
                    },
                    _ => { Ok(r) }
                }
            },
            (_, Some(rest)) => {
                match rest.
                    get("atom").
                    and_then(|a| atom(allocator, *a).ok()).
                    map(|atom| number_from_u8(allocator.buf(&atom)))
                {
                    Some(atom) => {
                        let node =
                            NodePath::new(Some(atom)).
                            add(NodePath::new(None).rest());
                        allocator.new_atom(node.as_path().data())
                    },
                    _ => { Ok(r) }
                }
            },
            _ => Ok(r)
        }
    };
}

fn quote_pattern_1(allocator: &mut Allocator) -> NodePtr {
    assemble(allocator, "(q . 0)").unwrap()
}

fn quote_null_optimizer(
    allocator: &mut Allocator,
    _memo: Rc<RefCell<HashMap<String, NodePtr>>>,
    r: NodePtr,
    _eval_f: Rc<dyn TRunProgram>,
) -> Result<NodePtr, EvalErr> {
    let quote_pattern_1 = quote_pattern_1(allocator);

    // This applies the transform `(q . 0)` => `0`
    let t1 = match_sexp(allocator, quote_pattern_1, r, HashMap::new());
    Ok(t1.map(|_| allocator.null()).unwrap_or_else(|| r))
}

fn apply_null_pattern_1(allocator: &mut Allocator) -> NodePtr {
    assemble(allocator, "(a 0 . (: . rest))").unwrap()
}

fn apply_null_optimizer(
    allocator: &mut Allocator,
    _memo: Rc<RefCell<HashMap<String, NodePtr>>>,
    r: NodePtr,
    _eval_f: Rc<dyn TRunProgram>,
) -> Result<NodePtr, EvalErr> {
    let apply_null_pattern_1 = apply_null_pattern_1(allocator);

    // This applies the transform `(a 0 ARGS)` => `0`
    let t1 = match_sexp(allocator, apply_null_pattern_1, r, HashMap::new());
    Ok(t1.map(|_| allocator.null()).unwrap_or_else(|| r))
}

struct OptimizerRunner<'a> {
    pub name: String,
    #[allow(clippy::type_complexity)]
    to_run: &'a dyn Fn(
        &mut Allocator,
        Rc<RefCell<HashMap<String, NodePtr>>>,
        NodePtr,
        Rc<dyn TRunProgram>,
    ) -> Result<NodePtr, EvalErr>,
}

impl<'a> OptimizerRunner<'a> {
    pub fn invoke(
        &self,
        allocator: &mut Allocator,
        optimized: Rc<RefCell<HashMap<String, NodePtr>>>,
        r: NodePtr,
        eval_f: Rc<dyn TRunProgram>,
    ) -> Result<NodePtr, EvalErr> {
        (self.to_run)(allocator, optimized, r, eval_f)
    }

    #[allow(clippy::type_complexity)]
    pub fn new(
        name: &str,
        to_run: &'a dyn Fn(
            &mut Allocator,
            Rc<RefCell<HashMap<String, NodePtr>>>,
            NodePtr,
            Rc<dyn TRunProgram>,
        ) -> Result<NodePtr, EvalErr>,
    ) -> Self {
        OptimizerRunner {
            name: name.to_string(),
            to_run,
        }
    }
}

pub fn optimize_sexp_(
    allocator: &mut Allocator,
    memo: Rc<RefCell<HashMap<String, NodePtr>>>,
    r_: NodePtr,
    eval_f: Rc<dyn TRunProgram>,
) -> Result<NodePtr, EvalErr> {
    let mut r = r_;
    let footprint = sha256tree(allocator, r).hex();

    {
        let memo_rc: &RefCell<HashMap<String, NodePtr>> = memo.borrow();
        let memo_ref: Ref<HashMap<String, NodePtr>> = memo_rc.borrow();
        let memo: &HashMap<String, NodePtr> = memo_ref.borrow();
        if let Some(res) = memo.get(&r_.to_string()) {
            return Ok(*res);
        }

        if let Some(res) = memo.get(&footprint) {
            return Ok(*res);
        }
    }

    /*
     * Optimize an s-expression R written for clvm to R_opt where
     * (a R args) == (a R_opt args) for ANY args.
     */
    let optimizers: Vec<OptimizerRunner> = vec![
        OptimizerRunner::new("cons_optimizer", &cons_optimizer),
        OptimizerRunner::new("constant_optimizer", &|allocator, memo, r, eval_f| {
            constant_optimizer(allocator, memo, r, 0, eval_f.clone())
        }),
        OptimizerRunner::new("cons_q_a_optimizer", &cons_q_a_optimizer),
        OptimizerRunner::new(
            "var_change_optimizer_cons_eval",
            &var_change_optimizer_cons_eval,
        ),
        OptimizerRunner::new("children_optimizer", &children_optimizer),
        OptimizerRunner::new("path_optimizer", &path_optimizer),
        OptimizerRunner::new("quote_null_optimizer", &quote_null_optimizer),
        OptimizerRunner::new("apply_null_optimizer", &apply_null_optimizer),
    ];

    loop {
        let start_r = r;
        let mut name = "".to_string();

        match allocator.sexp(r) {
            SExp::Atom(_) => {
                return Ok(r);
            }
            SExp::Pair(_, _) => {
                for opt in optimizers.iter() {
                    name = opt.name.clone();
                    match opt.invoke(allocator, memo.clone(), r, eval_f.clone()) {
                        Err(e) => {
                            return Err(e);
                        }
                        Ok(res) => {
                            if !equal_to(allocator, r, res) {
                                r = res;
                                break;
                            }
                        }
                    }
                }

                if equal_to(allocator, start_r, r) {
                    let memo_rc: &RefCell<HashMap<String, NodePtr>> = memo.borrow();
                    memo_rc.replace_with(|mr| {
                        let mut work = HashMap::new();
                        swap(&mut work, mr);
                        work.insert(footprint.clone(), start_r);
                        work.insert(r_.to_string(), start_r);
                        work
                    });

                    return Ok(start_r);
                }

                if DEBUG_OPTIMIZATIONS {
                    println!(
                        "{}OPT-{:?}[{}] => {}",
                        indent(),
                        name,
                        disassemble(allocator, start_r),
                        disassemble(allocator, r)
                    );
                }
            }
        }
    }
}

pub fn optimize_sexp(
    allocator: &mut Allocator,
    r: NodePtr,
    eval_f: Rc<dyn TRunProgram>,
) -> Result<NodePtr, EvalErr> {
    let optimized = Rc::new(RefCell::new(HashMap::new()));

    enter();
    if DIAG_OPTIMIZATIONS {
        println!("START OPTIMIZE {}", disassemble(allocator, r));
    }
    optimize_sexp_(allocator, optimized, r, eval_f).map(|x| {
        if DIAG_OPTIMIZATIONS {
            println!(
                "{}OPTIMIZE_SEXP {} GIVING {}",
                indent(),
                disassemble(allocator, r),
                disassemble(allocator, x)
            );
        }
        leave();
        x
    })
}

pub fn do_optimize(
    runner: Rc<dyn TRunProgram>,
    allocator: &mut Allocator,
    memo: Rc<RefCell<HashMap<String, NodePtr>>>,
    r: NodePtr,
) -> Response {
    let r_first = first(allocator, r)?;
    optimize_sexp_(allocator, memo, r_first, runner.clone())
        .map(|optimized| Reduction(1, optimized))
}
