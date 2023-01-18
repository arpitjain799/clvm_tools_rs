use std::rc::Rc;

use std::borrow::Borrow;

use crate::compiler::comptypes::{Binding, BodyForm, CompileForm, DefunData, HelperForm, LetData, LetFormKind, list_to_cons};
use crate::compiler::gensym::gensym;
use crate::compiler::sexp::{decode_string, SExp};
use crate::compiler::srcloc::Srcloc;


/* As in the python code, produce a pair whose (thanks richard)
 *
 *   - car is the compiled code and
 *   - cdr is the argument from the mod definition
 *
 *   Every let adds arguments, and since we model each function level target
 *   as a mod () in the end, we can do the same with let bindings, letting
 *   each group of bindings:
 *
 *       (mod args
 *         (let
 *           ((x (+ a 1))
 *            (y (+ b 1)))
 *
 *           (+ x y)))
 *
 *   Translate to:
 *
 *       (mod (a b)
 *         (defun let_$1 ((a b) x y) (+ x y))
 *         (let_$1 (r @) (+ a 1) (+ b 1))
 *         )
 */

fn cons_bodyform(loc: Srcloc, left: Rc<BodyForm>, right: Rc<BodyForm>) -> BodyForm {
    BodyForm::Call(
        loc.clone(),
        vec![
            Rc::new(BodyForm::Value(SExp::Atom(loc, "c".as_bytes().to_vec()))), // Cons
            left,
            right,
        ],
    )
}

/*
 * Produce a structure that mimics the expected environment if the current inline
 * context had been a function.
 */
fn create_let_env_expression(args: Rc<SExp>) -> BodyForm {
    match args.borrow() {
        SExp::Cons(l, a, b) => cons_bodyform(
            l.clone(),
            Rc::new(create_let_env_expression(a.clone())),
            Rc::new(create_let_env_expression(b.clone())),
        ),
        _ => {
            let cloned: &SExp = args.borrow();
            BodyForm::Value(cloned.clone())
        }
    }
}

fn generate_let_defun(
    l: Srcloc,
    kwl: Option<Srcloc>,
    name: &[u8],
    args: Rc<SExp>,
    bindings: Vec<Rc<Binding>>,
    body: Rc<BodyForm>,
) -> HelperForm {
    let new_arguments: Vec<Rc<SExp>> = bindings
        .iter()
        .map(|b| Rc::new(SExp::Atom(l.clone(), b.name.clone())))
        .collect();

    let inner_function_args = SExp::Cons(
        l.clone(),
        args,
        Rc::new(list_to_cons(l.clone(), &new_arguments)),
    );

    HelperForm::Defun(
        true,
        DefunData {
            loc: l.clone(),
            nl: l,
            kw: kwl,
            name: name.to_owned(),
            args: Rc::new(inner_function_args),
            body,
            synthetic: true
        },
    )
}

fn generate_let_args(_l: Srcloc, blist: Vec<Rc<Binding>>) -> Vec<Rc<BodyForm>> {
    blist.iter().map(|b| b.body.clone()).collect()
}

pub fn hoist_body_let_binding(
    outer_context: Option<Rc<SExp>>,
    args: Rc<SExp>,
    body: Rc<BodyForm>,
) -> (Vec<HelperForm>, Rc<BodyForm>) {
    match body.borrow() {
        BodyForm::Let(LetFormKind::Sequential, letdata) => {
            if letdata.bindings.is_empty() {
                return (vec![], letdata.body.clone());
            }

            // If we're here, we're in the middle of hoisting.
            // Simply slice one binding and do it again.
            let new_sub_expr = if letdata.bindings.len() == 1 {
                // There is one binding, so we just need to put body below
                letdata.body.clone()
            } else {
                // Slice other bindings
                let sub_bindings = letdata.bindings.iter().skip(1).cloned().collect();
                Rc::new(BodyForm::Let(
                    LetFormKind::Sequential,
                    LetData {
                        loc: letdata.loc.clone(),
                        kw: letdata.kw.clone(),
                        bindings: sub_bindings,
                        body: letdata.body.clone(),
                    },
                ))
            };

            hoist_body_let_binding(
                outer_context,
                args,
                Rc::new(BodyForm::Let(
                    LetFormKind::Parallel,
                    LetData {
                        loc: letdata.loc.clone(),
                        kw: letdata.kw.clone(),
                        bindings: vec![letdata.bindings[0].clone()],
                        body: new_sub_expr,
                    },
                )),
            )
        }
        BodyForm::Let(LetFormKind::Parallel, letdata) => {
            let mut out_defuns = Vec::new();
            let defun_name = gensym("letbinding".as_bytes().to_vec());

            let mut revised_bindings = Vec::new();
            for b in letdata.bindings.iter() {
                let (mut new_helpers, new_binding) = hoist_body_let_binding(
                    outer_context.clone(),
                    args.clone(),
                    b.body.clone(),
                );
                out_defuns.append(&mut new_helpers);
                revised_bindings.push(Rc::new(Binding {
                    loc: b.loc.clone(),
                    nl: b.nl.clone(),
                    name: b.name.clone(),
                    body: new_binding,
                }));
            }

            let generated_defun = generate_let_defun(
                letdata.loc.clone(),
                None,
                &defun_name,
                args,
                revised_bindings.to_vec(),
                letdata.body.clone(),
            );
            out_defuns.push(generated_defun);

            let mut let_args = generate_let_args(letdata.loc.clone(), revised_bindings.to_vec());
            let pass_env = outer_context
                .map(create_let_env_expression)
                .unwrap_or_else(|| {
                    BodyForm::Call(
                        letdata.loc.clone(),
                        vec![
                            Rc::new(BodyForm::Value(SExp::Atom(
                                letdata.loc.clone(),
                                "r".as_bytes().to_vec(),
                            ))),
                            Rc::new(BodyForm::Value(SExp::Atom(
                                letdata.loc.clone(),
                                "@".as_bytes().to_vec(),
                            ))),
                        ],
                    )
                });

            let mut call_args = vec![
                Rc::new(BodyForm::Value(SExp::Atom(letdata.loc.clone(), defun_name))),
                Rc::new(pass_env),
            ];
            call_args.append(&mut let_args);

            let final_call = BodyForm::Call(letdata.loc.clone(), call_args);
            (out_defuns, Rc::new(final_call))
        }
        BodyForm::Call(l, list) => {
            let mut vres = Vec::new();
            let mut new_call_list = vec![list[0].clone()];
            for i in list.iter().skip(1) {
                let (new_helper, new_arg) = hoist_body_let_binding(
                    outer_context.clone(),
                    args.clone(),
                    i.clone(),
                );
                new_call_list.push(new_arg);
                vres.append(&mut new_helper.clone());
            }
            (vres, Rc::new(BodyForm::Call(l.clone(), new_call_list)))
        }
        _ => (Vec::new(), body.clone()),
    }
}

pub fn process_helper_let_bindings(
    helpers: &[HelperForm],
) -> Vec<HelperForm> {
    let mut result = helpers.to_owned();
    let mut i = 0;

    while i < result.len() {
        match result[i].clone() {
            HelperForm::Defun(inline, defun) => {
                let context = if inline {
                    Some(defun.args.clone())
                } else {
                    None
                };
                let helper_result = hoist_body_let_binding(
                    context,
                    defun.args.clone(),
                    defun.body.clone(),
                );
                let hoisted_helpers = helper_result.0;
                let hoisted_body = helper_result.1.clone();

                result[i] = HelperForm::Defun(
                    inline,
                    DefunData {
                        loc: defun.loc.clone(),
                        nl: defun.nl.clone(),
                        kw: defun.kw.clone(),
                        name: defun.name.clone(),
                        args: defun.args.clone(),
                        body: hoisted_body,
                        synthetic: true
                    },
                );

                i += 1;

                for (j, hh) in hoisted_helpers.iter().enumerate() {
                    result.insert(i + j, hh.clone());
                }
            }
            _ => {
                i += 1;
            }
        }
    }

    result
}

fn print_helpers(helpers: &[HelperForm]) {
    let names: Vec<String> = helpers.iter().map(|h| decode_string(h.name())).collect();
    eprintln!("names: {:?}", names);
}

pub fn hoist_let_forms(
    mut compileform: CompileForm
) -> CompileForm {
    print_helpers(&compileform.helpers);
    let mut new_helpers = process_helper_let_bindings(&compileform.helpers);
    compileform.helpers = new_helpers;
    print_helpers(&compileform.helpers);
    let (new_helpers_from_body, expr) =
        hoist_body_let_binding(None, compileform.args.clone(), compileform.exp);
    let mut expanded_helpers = process_helper_let_bindings(&new_helpers_from_body);
    compileform.helpers.append(&mut expanded_helpers);
    print_helpers(&compileform.helpers);
    compileform.exp = expr;
    compileform
}
