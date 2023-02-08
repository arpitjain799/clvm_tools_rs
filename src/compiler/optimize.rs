use num_bigint::ToBigInt;

use std::borrow::Borrow;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use clvm_rs::allocator::Allocator;

use crate::classic::clvm::__type_compatibility__::bi_one;
use crate::classic::clvm::__type_compatibility__::bi_zero;
use crate::classic::clvm_tools::node_path::compose_paths;
use crate::classic::clvm_tools::stages::stage_0::TRunProgram;

use crate::compiler::clvm::run;
use crate::compiler::codegen::{codegen, get_callable};
use crate::compiler::comptypes::{
    BodyForm, Callable, CompileErr, CompileForm, CompilerOpts, HelperForm, PrimaryCodegen,
};
use crate::compiler::evaluate::{build_reflex_captures, Evaluator, ExpandMode, EVAL_STACK_LIMIT};
#[cfg(test)]
use crate::compiler::sexp::parse_sexp;
use crate::compiler::sexp::SExp;
use crate::compiler::srcloc::Srcloc;
use crate::util::u8_from_number;

const CONST_FOLD_LIMIT: usize = 10000000;

fn is_at_form(head: Rc<BodyForm>) -> bool {
    match head.borrow() {
        BodyForm::Value(SExp::Atom(_, a)) => a.len() == 1 && a[0] == b'@',
        _ => false,
    }
}

// Return a score for sexp size.
pub fn sexp_scale(sexp: &SExp) -> u64 {
    match sexp {
        SExp::Cons(_, a, b) => {
            let a_scale = sexp_scale(a.borrow());
            let b_scale = sexp_scale(b.borrow());
            1_u64 + a_scale + b_scale
        }
        SExp::Nil(_) => 1,
        SExp::QuotedString(_, _, s) => 1_u64 + s.len() as u64,
        SExp::Atom(_, n) => 1_u64 + n.len() as u64,
        SExp::Integer(_, i) => {
            let raw_bits = i.bits();
            let use_bits = if raw_bits > 0 { raw_bits - 1 } else { 0 };
            let bytes = use_bits / 8;
            1_u64 + bytes
        }
    }
}

#[test]
fn test_sexp_scale_increases_with_atom_size() {
    let l = Srcloc::start("*test*");
    assert!(
        sexp_scale(&SExp::Integer(l.clone(), bi_one()))
            < sexp_scale(&SExp::Integer(l, 1000000_u32.to_bigint().unwrap()))
    );
}

pub fn optimize_expr(
    allocator: &mut Allocator,
    opts: Rc<dyn CompilerOpts>,
    runner: Rc<dyn TRunProgram>,
    compiler: &PrimaryCodegen,
    body: Rc<BodyForm>,
) -> Option<(bool, Rc<BodyForm>)> {
    match body.borrow() {
        BodyForm::Quoted(_) => Some((true, body)),
        BodyForm::Call(l, forms) => {
            // () evaluates to ()
            if forms.is_empty() {
                return Some((true, body));
            } else if is_at_form(forms[0].clone()) {
                return None;
            }

            let mut examine_call = |al: Srcloc, an: &Vec<u8>| {
                get_callable(
                    opts.clone(),
                    compiler,
                    l.clone(),
                    Rc::new(SExp::Atom(al, an.to_vec())),
                )
                .map(|calltype| match calltype {
                    // A macro invocation emits a bodyform, which we
                    // run back through the frontend and check.
                    Callable::CallMacro(_l, _) => None,
                    // A function is constant if its body is a constant
                    // expression or all its arguments are constant and
                    // its body doesn't include an environment reference.
                    Callable::CallDefun(_l, _target) => None,
                    // A primcall is constant if its arguments are constant
                    Callable::CallPrim(l, _) => {
                        let mut constant = true;
                        let optimized_args: Vec<(bool, Rc<BodyForm>)> = forms
                            .iter()
                            .skip(1)
                            .map(|a| {
                                let optimized = optimize_expr(
                                    allocator,
                                    opts.clone(),
                                    runner.clone(),
                                    compiler,
                                    a.clone(),
                                );
                                constant = constant
                                    && optimized.as_ref().map(|x| x.0).unwrap_or_else(|| false);
                                optimized
                                    .map(|x| (x.0, x.1))
                                    .unwrap_or_else(|| (false, a.clone()))
                            })
                            .collect();

                        let mut result_list = vec![forms[0].clone()];
                        let mut replaced_args =
                            optimized_args.iter().map(|x| x.1.clone()).collect();
                        result_list.append(&mut replaced_args);
                        let code = BodyForm::Call(l.clone(), result_list);

                        if constant {
                            run(
                                allocator,
                                runner.clone(),
                                opts.prim_map(),
                                code.to_sexp(),
                                Rc::new(SExp::Nil(l)),
                                Some(CONST_FOLD_LIMIT),
                            )
                            .map(|x| {
                                let x_borrow: &SExp = x.borrow();
                                Some((true, Rc::new(BodyForm::Quoted(x_borrow.clone()))))
                            })
                            .unwrap_or_else(|_| Some((false, Rc::new(code))))
                        } else {
                            Some((false, Rc::new(code)))
                        }
                    }
                    _ => None,
                })
                .unwrap_or_else(|_| None)
            };

            match forms[0].borrow() {
                BodyForm::Value(SExp::Integer(al, an)) => {
                    examine_call(al.clone(), &u8_from_number(an.clone()))
                }
                BodyForm::Value(SExp::QuotedString(al, _, an)) => examine_call(al.clone(), an),
                BodyForm::Value(SExp::Atom(al, an)) => examine_call(al.clone(), an),
                _ => None,
            }
        }
        BodyForm::Value(SExp::Integer(l, i)) => Some((
            true,
            Rc::new(BodyForm::Quoted(SExp::Integer(l.clone(), i.clone()))),
        )),
        _ => None,
    }
}

// If (1) appears anywhere outside of a quoted expression, it can be replaced with
// () since nil yields itself.
fn null_optimization(sexp: Rc<SExp>, spine: bool) -> (bool, Rc<SExp>) {
    if let SExp::Cons(l, a, b) = sexp.borrow() {
        if let SExp::Atom(_, name) = a.atomize() {
            if (name == vec![1] || name == b"q") && !spine {
                let b_empty = match b.borrow() {
                    SExp::Atom(_, tail) => tail.is_empty(),
                    SExp::QuotedString(_, _, q) => q.is_empty(),
                    SExp::Integer(_, i) => *i == bi_zero(),
                    SExp::Nil(_) => true,
                    _ => false,
                };

                if b_empty {
                    return (true, b.clone());
                } else {
                    return (false, sexp);
                }
            }
        }

        let (oa, opt_a) = null_optimization(a.clone(), false);
        let (ob, opt_b) = null_optimization(b.clone(), true);

        if oa || ob {
            return (true, Rc::new(SExp::Cons(l.clone(), opt_a, opt_b)));
        }
    }

    (false, sexp)
}

#[test]
fn test_null_optimization_basic() {
    let loc = Srcloc::start("*test*");
    let parsed = parse_sexp(loc.clone(), "(2 (1 1) (4 (1) 1))".bytes()).expect("should parse");
    let (did_work, optimized) = null_optimization(parsed[0].clone(), true);
    assert!(did_work);
    assert_eq!(optimized.to_string(), "(2 (1 1) (4 () 1))");
}

#[test]
fn test_null_optimization_skips_quoted() {
    let loc = Srcloc::start("*test*");
    let parsed = parse_sexp(loc.clone(), "(2 (1 (1) (1) (1)) (1))".bytes()).expect("should parse");
    let (did_work, optimized) = null_optimization(parsed[0].clone(), true);
    assert!(did_work);
    assert_eq!(optimized.to_string(), "(2 (1 (1) (1) (1)) ())");
}

#[test]
fn test_null_optimization_ok_not_doing_anything() {
    let loc = Srcloc::start("*test*");
    let parsed = parse_sexp(loc.clone(), "(2 (1 (1) (1) (1)) (3))".bytes()).expect("should parse");
    let (did_work, optimized) = null_optimization(parsed[0].clone(), true);
    assert!(!did_work);
    assert_eq!(optimized.to_string(), "(2 (1 (1) (1) (1)) (3))");
}

// Do final optimizations on the finished CLVM code.
// These should be lightweight transformations that save space.
pub fn finish_optimization(sexp: &SExp) -> SExp {
    let (did_work, optimized) = null_optimization(Rc::new(sexp.clone()), false);
    let (also_worked, more_optimized) = brief_path_selection(optimized.clone());
    if did_work || also_worked {
        let o_borrowed: &SExp = more_optimized.borrow();
        o_borrowed.clone()
    } else {
        sexp.clone()
    }
}

fn take_smaller_form(
    allocator: &mut Allocator,
    runner: Rc<dyn TRunProgram>,
    opts: Rc<dyn CompilerOpts>,
    compileform: &CompileForm,
    optimized_helpers: &[HelperForm],
    body: Rc<BodyForm>,
    with_inlines: bool,
) -> Result<CompileForm, CompileErr> {
    let new_evaluator = Evaluator::new(opts.clone(), runner.clone(), optimized_helpers.to_vec());

    let mut env = HashMap::new();
    build_reflex_captures(&mut env, compileform.args.clone());

    let shrunk = new_evaluator.shrink_bodyform(
        allocator,
        compileform.args.clone(),
        &env,
        body.clone(),
        ExpandMode {
            functions: false,
            lets: with_inlines,
        },
        Some(EVAL_STACK_LIMIT),
    )?;

    let normal_form = CompileForm {
        loc: compileform.loc.clone(),
        include_forms: compileform.include_forms.clone(),
        args: compileform.args.clone(),
        helpers: optimized_helpers.to_vec(),
        exp: body,
        ty: compileform.ty.clone(),
    };
    let shrunk_form = CompileForm {
        loc: compileform.loc.clone(),
        include_forms: compileform.include_forms.clone(),
        args: compileform.args.clone(),
        helpers: optimized_helpers.to_vec(),
        exp: shrunk,
        ty: compileform.ty.clone(),
    };
    let mut phantom_table = HashMap::new();
    let generated_shrunk = codegen(
        allocator,
        runner.clone(),
        opts.clone(),
        &shrunk_form,
        &mut phantom_table,
    )?;
    let generated_normal = codegen(
        allocator,
        runner.clone(),
        opts.clone(),
        &normal_form,
        &mut phantom_table,
    )?;
    let normal_scale = sexp_scale(&generated_normal);
    let shrunk_scale = sexp_scale(&generated_shrunk);
    if normal_scale < shrunk_scale {
        Ok(normal_form)
    } else {
        Ok(shrunk_form)
    }
}

pub fn fe_opt(
    allocator: &mut Allocator,
    runner: Rc<dyn TRunProgram>,
    opts: Rc<dyn CompilerOpts>,
    compileform: &CompileForm,
    with_inlines: bool,
) -> Result<CompileForm, CompileErr> {
    let mut compiler_helpers = compileform.helpers.clone();
    let mut used_names = HashSet::new();

    if !opts.in_defun() {
        for c in compileform.helpers.iter() {
            used_names.insert(c.name().clone());
        }

        for helper in (opts
            .compiler()
            .map(|c| c.orig_help)
            .unwrap_or_else(Vec::new))
        .iter()
        {
            if !used_names.contains(helper.name()) {
                compiler_helpers.push(helper.clone());
            }
        }
    }

    let mut optimized_helpers: Vec<HelperForm> = Vec::new();
    for h in compiler_helpers.iter() {
        if let HelperForm::Defun(false, defun) = &h {
            let ref_compileform = CompileForm {
                loc: compileform.loc.clone(),
                include_forms: compileform.include_forms.clone(),
                args: defun.args.clone(),
                helpers: compileform.helpers.clone(),
                exp: defun.body.clone(),
                ty: defun.ty.clone(),
            };
            let better_form = take_smaller_form(
                allocator,
                runner.clone(),
                opts.clone(),
                &ref_compileform,
                &ref_compileform.helpers,
                defun.body.clone(),
                with_inlines,
            )?;

            let mut new_defun = defun.clone();
            new_defun.body = better_form.exp.clone();
            optimized_helpers.push(HelperForm::Defun(false, new_defun));
        } else {
            optimized_helpers.push(h.clone());
        }
    }

    take_smaller_form(
        allocator,
        runner,
        opts,
        compileform,
        &optimized_helpers,
        compileform.exp.clone(),
        with_inlines,
    )
}

fn is_quote_atom(a: &SExp) -> bool {
    if let SExp::Atom(_,n) = a.atomize() {
        return n.len() == 1 && n[0] == 1;
    }

    false
}

fn is_first_atom(a: &SExp) -> bool {
    if let SExp::Atom(_,n) = a.atomize() {
        return n.len() == 1 && n[0] == 5;
    }

    false
}

fn is_rest_atom(a: &SExp) -> bool {
    if let SExp::Atom(_,n) = a.atomize() {
        return n.len() == 1 && n[0] == 6;
    }

    false
}

// Collapse sequences of (f (r (f ... X))) representing (a P1 X)
// into (a P1 X) if X is not a path
// or P1 || X    if X is a path
fn brief_path_selection_single(mut body: Rc<SExp>) -> (bool, Rc<SExp>) {
    let orig_body = body.clone();
    let mut found_stack = 0;
    let mut target_path = bi_one();

    while let Some(lst) = body.proper_list() {
        if let [cmd, arg] = &lst[..] {
            if is_quote_atom(&cmd) {
                break;
            }

            let is_first = is_first_atom(&cmd);
            if is_first || is_rest_atom(&cmd) {
                found_stack += 1;
                target_path *= 2_u32.to_bigint().unwrap();
                if !is_first {
                    target_path += bi_one();
                }
                body = Rc::new(arg.clone());
                continue;
            }
        }

        break;
    }

    if found_stack > 0 {
        let intval =
            if let SExp::Integer(_l,i) = body.borrow() {
                Some(i.clone())
            } else {
                None
            };

        if let Some(i) = intval {
            // The number to the left is "closer to the root".
            // These bits are selected first.
            let final_path = compose_paths(&i, &target_path);
            return (true, Rc::new(SExp::Integer(body.loc(), final_path)));
        }

        /*
        if found_stack > 2 {
            let apply_atom = Rc::new(SExp::Atom(body.loc(), vec![2]));
            let at_atom = Rc::new(SExp::Atom(body.loc(), vec![1]));

            return (true, Rc::new(SExp::Cons(
                body.loc(),
                apply_atom.clone(),
                Rc::new(SExp::Cons(
                    body.loc(),
                    Rc::new(SExp::Cons(
                        body.loc(),
                        at_atom.clone(),
                        Rc::new(SExp::Integer(body.loc(), target_path))
                    )),
                    Rc::new(SExp::Cons(
                        body.loc(),
                        Rc::new(SExp::Cons(
                            body.loc(),
                            apply_atom,
                            Rc::new(SExp::Cons(
                                body.loc(),
                                body.clone(),
                                Rc::new(SExp::Cons(
                                    body.loc(),
                                    at_atom.clone(),
                                    Rc::new(SExp::Nil(body.loc()))
                                ))
                            ))
                        )),
                        Rc::new(SExp::Nil(body.loc()))
                    ))
                ))
            )));
        }
        */
    }

    (false, orig_body)
}

pub fn brief_path_selection(body: Rc<SExp>) -> (bool, Rc<SExp>) {
    let (changed, new_body) = brief_path_selection_single(body.clone());
    if changed {
        return (true, new_body);
    }

    if let Some(lst) = body.proper_list() {
        if lst.len() < 2 || is_quote_atom(&lst[0]) {
            return (false, body);
        }

        let mut end = Rc::new(SExp::Nil(body.loc()));
        let mut updated = false;
        for f in lst.iter().rev() {
            let (mod_a, a) = brief_path_selection(Rc::new(f.clone()));
            updated = updated || mod_a;
            end = Rc::new(SExp::Cons(body.loc(), a, end));
        }
        return (updated, end);
    }

    (false, body)
}

#[test]
fn test_brief_path_selection_none() {
    let filename = "*test*";
    let loc = Srcloc::start(filename);
    let parsed = parse_sexp(loc.clone(), "(2 (1 (1) (1) (1)) (3))".bytes()).expect("should parse");
    let (did_work, optimized) = brief_path_selection(parsed[0].clone());
    assert!(!did_work);
    assert_eq!(optimized.to_string(), "(2 (1 (1) (1) (1)) (3))");
}

/*
#[test]
fn test_brief_path_selection_one_first_unknown_body() {
    let filename = "*test*";
    let loc = Srcloc::start(filename);
    let parsed = parse_sexp(loc.clone(), "(5 (5 (5 (+ 3 2))))".bytes()).expect("should parse");
    let (did_work, optimized) = brief_path_selection(parsed[0].clone());
    assert!(did_work);
    assert_eq!(optimized.to_string(), "(2 8 (2 (+ 3 2) 1))");
}
*/

#[test]
fn test_brief_path_selection_one_first_path_body_1() {
    let filename = "*test*";
    let loc = Srcloc::start(filename);
    let parsed = parse_sexp(loc.clone(), "(5 (5 (6 (5 11))))".bytes()).expect("should parse");
    let (did_work, optimized) = brief_path_selection(parsed[0].clone());
    assert!(did_work);
    // f f r f -> path 18 (0100[1])
    // 11      ->         (110[1])
    // Joined so that 11's path occupies the inner (low) bits: 1100100[1]
    assert_eq!(optimized.to_string(), "147");
}
