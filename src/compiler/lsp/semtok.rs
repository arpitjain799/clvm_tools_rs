use std::borrow::Borrow;
use std::cmp::Ordering;
use std::collections::{BTreeMap, HashMap};
use std::rc::Rc;

use lsp_server::{
    RequestId,
    Response
};
use lsp_types::{
    SemanticToken,
    SemanticTokens
};

use crate::compiler::comptypes::{
    BodyForm,
    CompileForm,
    HelperForm,
    LetFormKind
};
use crate::compiler::lsp::{
    TK_FUNCTION_IDX,
    TK_MACRO_IDX,
    TK_NUMBER_IDX,
    TK_PARAMETER_IDX,
    TK_STRING_IDX,
    TK_VARIABLE_IDX,
    TK_DEFINITION_BIT,
    TK_READONLY_BIT
};
use crate::compiler::sexp::{SExp, decode_string};
use crate::compiler::srcloc::Srcloc;

#[derive(Clone, Debug)]
pub struct SemanticTokenSortable {
    pub loc: Srcloc,
    pub token_type: u32,
    pub token_mod: u32,
}

impl PartialEq for SemanticTokenSortable {
    fn eq(&self, other: &SemanticTokenSortable) -> bool {
        self.loc.file == other.loc.file && self.loc.line == other.loc.line && self.loc.col == other.loc.col
    }
}

impl Eq for SemanticTokenSortable {
}

impl PartialOrd for SemanticTokenSortable {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let cf = self.loc.file.cmp(other.loc.file.borrow());
        if cf != Ordering::Equal {
            return Some(cf);
        }
        let lf = self.loc.line.cmp(&other.loc.line);
        if lf != Ordering::Equal {
            return Some(lf);
        }
        Some(self.loc.col.cmp(&other.loc.col))
    }
}

impl Ord for SemanticTokenSortable {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

fn collect_arg_tokens(
    collected_tokens: &mut Vec<SemanticTokenSortable>,
    argcollection: &mut HashMap<Vec<u8>, Srcloc>,
    args: Rc<SExp>
) {
    match args.borrow() {
        SExp::Atom(l,a) => {
            argcollection.insert(a.clone(), l.clone());
            collected_tokens.push(SemanticTokenSortable {
                loc: l.clone(),
                token_type: TK_PARAMETER_IDX,
                token_mod: 1 << TK_DEFINITION_BIT
            });
        },
        SExp::Cons(_,a,b) => {
            collect_arg_tokens(collected_tokens, argcollection, a.clone());
            collect_arg_tokens(collected_tokens, argcollection, b.clone());
        }
        _ => { }
    }
}

fn process_body_code(
    collected_tokens: &mut Vec<SemanticTokenSortable>,
    gotodef: &mut BTreeMap<SemanticTokenSortable, Srcloc>,
    argcollection: &HashMap<Vec<u8>, Srcloc>,
    varcollection: &HashMap<Vec<u8>, Srcloc>,
    frontend: &CompileForm,
    body: Rc<BodyForm>
) {
    match body.borrow() {
        BodyForm::Let(_,k,bindings,inner_body) => {
            let mut bindings_vars = varcollection.clone();
            for b in bindings.clone() {
                collected_tokens.push(SemanticTokenSortable {
                    loc: b.nl.clone(),
                    token_type: TK_VARIABLE_IDX,
                    token_mod: 1 << TK_DEFINITION_BIT | 1 << TK_READONLY_BIT
                });
                if k == &LetFormKind::Sequential {
                    // Bindings above affect code below
                    process_body_code(
                        collected_tokens,
                        gotodef,
                        argcollection,
                        &bindings_vars,
                        frontend,
                        b.body.clone()
                    );
                } else {
                    process_body_code(
                        collected_tokens,
                        gotodef,
                        argcollection,
                        varcollection,
                        frontend,
                        b.body.clone()
                    )
                }
                bindings_vars.insert(b.name.clone(), b.nl.clone());
            }
            process_body_code(
                collected_tokens,
                gotodef,
                argcollection,
                &bindings_vars,
                frontend,
                inner_body.clone()
            );
        },
        BodyForm::Quoted(SExp::Integer(l,_)) => {
            collected_tokens.push(SemanticTokenSortable {
                loc: l.clone(),
                token_type: TK_NUMBER_IDX,
                token_mod: 0
            });
        },
        BodyForm::Quoted(SExp::QuotedString(l,_,_)) => {
            collected_tokens.push(SemanticTokenSortable {
                loc: l.clone(),
                token_type: TK_STRING_IDX,
                token_mod: 0,
            });
        },
        BodyForm::Value(SExp::Atom(l,a)) => {
            if let Some(argloc) = argcollection.get(a) {
                let t = SemanticTokenSortable {
                    loc: l.clone(),
                    token_type: TK_PARAMETER_IDX,
                    token_mod: 0
                };
                collected_tokens.push(t.clone());
                gotodef.insert(t, argloc.clone());
            }
            if let Some(varloc) = varcollection.get(a) {
                let t = SemanticTokenSortable {
                    loc: l.clone(),
                    token_type: TK_VARIABLE_IDX,
                    token_mod: 0
                };
                collected_tokens.push(t.clone());
                gotodef.insert(t, varloc.clone());
            }
        },
        BodyForm::Value(SExp::Integer(l,_)) => {
            collected_tokens.push(SemanticTokenSortable {
                loc: l.clone(),
                token_type: TK_NUMBER_IDX,
                token_mod: 0
            });
        },
        BodyForm::Call(_, args) => {
            if args.len() == 0 {
                return;
            }

            let head: &BodyForm = args[0].borrow();
            if let BodyForm::Value(SExp::Atom(l,a)) = head {
                for f in frontend.helpers.iter() {
                    match f {
                        HelperForm::Defun(_inline, defun) => {
                            if &defun.name == a {
                                let st = SemanticTokenSortable {
                                    loc: l.clone(),
                                    token_type: TK_FUNCTION_IDX,
                                    token_mod: 0
                                };
                                gotodef.insert(st.clone(), defun.nl.clone());
                                collected_tokens.push(st);
                                break;
                            }
                        },
                        HelperForm::Defmacro(mac) => {
                            if &mac.name == a {
                                let st = SemanticTokenSortable {
                                    loc: l.clone(),
                                    token_type: TK_MACRO_IDX,
                                    token_mod: 0
                                };
                                gotodef.insert(st.clone(), mac.nl.clone());
                                collected_tokens.push(st);
                                break;
                            }
                        },
                        _ => { }
                    }
                }
            }

            for a in args.iter().skip(1) {
                process_body_code(
                    collected_tokens,
                    gotodef,
                    argcollection,
                    varcollection,
                    frontend,
                    a.clone()
                );
            }
        },
        _ => { }
    }
}

pub fn do_semantic_tokens(
    id: RequestId,
    uristring: &String,
    goto_def: &mut BTreeMap<SemanticTokenSortable, Srcloc>,
    frontend: &CompileForm
) -> Response {
    let mut collected_tokens = Vec::new();
    let varcollection = HashMap::new();
    for form in frontend.helpers.iter() {
        match form {
            HelperForm::Defun(l,defun) => {
                eprintln!("helperform range {}: {}", decode_string(&defun.name), defun.loc.to_string());
                let mut argcollection = HashMap::new();
                collected_tokens.push(SemanticTokenSortable {
                    loc: defun.nl.clone(),
                    token_type: TK_FUNCTION_IDX,
                    token_mod: 1 << TK_DEFINITION_BIT
                });
                collect_arg_tokens(
                    &mut collected_tokens,
                    &mut argcollection,
                    defun.args.clone()
                );
                process_body_code(
                    &mut collected_tokens,
                    goto_def,
                    &argcollection,
                    &varcollection,
                    frontend,
                    defun.body.clone()
                );
            },
            _ => { }
        }
    }

    let mut argcollection = HashMap::new();
    collect_arg_tokens(
        &mut collected_tokens,
        &mut argcollection,
        frontend.args.clone()
    );
    process_body_code(
        &mut collected_tokens,
        goto_def,
        &argcollection,
        &varcollection,
        frontend,
        frontend.exp.clone()
    );

    collected_tokens = collected_tokens.iter().filter(|t| {
        let borrowed: &String = t.loc.file.borrow();
        borrowed == uristring
    }).cloned().collect();
    collected_tokens.sort();
    let mut result_tokens = SemanticTokens {
        result_id: None,
        data: Vec::new(),
    };

    let mut last_row = 1;
    let mut last_col = 1;

    for t in collected_tokens.iter() {
        if t.loc.line < last_row || (t.loc.line == last_row && t.loc.col <= last_col) {
            continue;
        }
        if t.loc.line != last_row {
            last_col = 1;
        }
        result_tokens.data.push(SemanticToken {
            delta_line: (t.loc.line - last_row) as u32,
            delta_start: (t.loc.col - last_col) as u32,
            length: t.loc.len() as u32,
            token_type: t.token_type,
            token_modifiers_bitset: t.token_mod
        });
        last_row = t.loc.line;
        last_col = t.loc.col;
    }

    Response {
        id,
        error: None,
        result: Some(serde_json::to_value(result_tokens).unwrap())
    }
}
