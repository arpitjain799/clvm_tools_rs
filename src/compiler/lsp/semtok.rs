use std::borrow::Borrow;
use std::cmp::Ordering;
use std::collections::{BTreeMap, HashMap};
use std::rc::Rc;

use lsp_server::{Message, RequestId, Response};
use lsp_types::{SemanticToken, SemanticTokens, SemanticTokensParams};

use crate::compiler::comptypes::{BodyForm, CompileForm, HelperForm, LetFormKind};
use crate::compiler::lsp::types::{DocPosition, DocRange, LSPServiceProvider};
use crate::compiler::lsp::{
    TK_COMMENT_IDX, TK_DEFINITION_BIT, TK_FUNCTION_IDX, TK_KEYWORD_IDX, TK_MACRO_IDX,
    TK_NUMBER_IDX, TK_PARAMETER_IDX, TK_READONLY_BIT, TK_STRING_IDX, TK_VARIABLE_IDX,
};
use crate::compiler::sexp::SExp;
use crate::compiler::srcloc::Srcloc;

#[derive(Clone, Debug)]
pub struct SemanticTokenSortable {
    pub loc: Srcloc,
    pub token_type: u32,
    pub token_mod: u32,
}

impl PartialEq for SemanticTokenSortable {
    fn eq(&self, other: &SemanticTokenSortable) -> bool {
        self.loc.file == other.loc.file
            && self.loc.line == other.loc.line
            && self.loc.col == other.loc.col
    }
}

impl Eq for SemanticTokenSortable {}

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

pub trait LSPSemtokRequestHandler {
    fn handle_semantic_tokens(
        &mut self,
        id: RequestId,
        params: &SemanticTokensParams,
    ) -> Result<Vec<Message>, String>;
}

fn collect_arg_tokens(
    collected_tokens: &mut Vec<SemanticTokenSortable>,
    argcollection: &mut HashMap<Vec<u8>, Srcloc>,
    args: Rc<SExp>,
) {
    match args.borrow() {
        SExp::Atom(l, a) => {
            argcollection.insert(a.clone(), l.clone());
            collected_tokens.push(SemanticTokenSortable {
                loc: l.clone(),
                token_type: TK_PARAMETER_IDX,
                token_mod: 1 << TK_DEFINITION_BIT,
            });
        }
        SExp::Cons(_, a, b) => {
            collect_arg_tokens(collected_tokens, argcollection, a.clone());
            collect_arg_tokens(collected_tokens, argcollection, b.clone());
        }
        _ => {}
    }
}

fn process_body_code(
    collected_tokens: &mut Vec<SemanticTokenSortable>,
    gotodef: &mut BTreeMap<SemanticTokenSortable, Srcloc>,
    argcollection: &HashMap<Vec<u8>, Srcloc>,
    varcollection: &HashMap<Vec<u8>, Srcloc>,
    frontend: &CompileForm,
    body: Rc<BodyForm>,
) {
    match body.borrow() {
        BodyForm::Let(k, letdata) => {
            let mut bindings_vars = varcollection.clone();
            for b in letdata.bindings.iter() {
                collected_tokens.push(SemanticTokenSortable {
                    loc: b.nl.clone(),
                    token_type: TK_VARIABLE_IDX,
                    token_mod: 1 << TK_DEFINITION_BIT | 1 << TK_READONLY_BIT,
                });
                if k == &LetFormKind::Sequential {
                    // Bindings above affect code below
                    process_body_code(
                        collected_tokens,
                        gotodef,
                        argcollection,
                        &bindings_vars,
                        frontend,
                        b.body.clone(),
                    );
                } else {
                    process_body_code(
                        collected_tokens,
                        gotodef,
                        argcollection,
                        varcollection,
                        frontend,
                        b.body.clone(),
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
                letdata.body.clone(),
            );
        }
        BodyForm::Quoted(SExp::Integer(l, _)) => {
            collected_tokens.push(SemanticTokenSortable {
                loc: l.clone(),
                token_type: TK_NUMBER_IDX,
                token_mod: 0,
            });
        }
        BodyForm::Quoted(SExp::QuotedString(l, _, _)) => {
            collected_tokens.push(SemanticTokenSortable {
                loc: l.clone(),
                token_type: TK_STRING_IDX,
                token_mod: 0,
            });
        }
        BodyForm::Value(SExp::Atom(l, a)) => {
            if let Some(argloc) = argcollection.get(a) {
                let t = SemanticTokenSortable {
                    loc: l.clone(),
                    token_type: TK_PARAMETER_IDX,
                    token_mod: 0,
                };
                collected_tokens.push(t.clone());
                gotodef.insert(t, argloc.clone());
            }
            if let Some(varloc) = varcollection.get(a) {
                let t = SemanticTokenSortable {
                    loc: l.clone(),
                    token_type: TK_VARIABLE_IDX,
                    token_mod: 0,
                };
                collected_tokens.push(t.clone());
                gotodef.insert(t, varloc.clone());
            }
        }
        BodyForm::Value(SExp::Integer(l, _)) => {
            collected_tokens.push(SemanticTokenSortable {
                loc: l.clone(),
                token_type: TK_NUMBER_IDX,
                token_mod: 0,
            });
        }
        BodyForm::Call(_, args) => {
            if args.is_empty() {
                return;
            }

            let head: &BodyForm = args[0].borrow();
            if let BodyForm::Value(SExp::Atom(l, a)) = head {
                for f in frontend.helpers.iter() {
                    match f {
                        HelperForm::Defun(_inline, defun) => {
                            if &defun.name == a {
                                let st = SemanticTokenSortable {
                                    loc: l.clone(),
                                    token_type: TK_FUNCTION_IDX,
                                    token_mod: 0,
                                };
                                gotodef.insert(st.clone(), defun.nl.clone());
                                collected_tokens.push(st);
                                break;
                            }
                        }
                        HelperForm::Defmacro(mac) => {
                            if &mac.name == a {
                                let st = SemanticTokenSortable {
                                    loc: l.clone(),
                                    token_type: TK_MACRO_IDX,
                                    token_mod: 0,
                                };
                                gotodef.insert(st.clone(), mac.nl.clone());
                                collected_tokens.push(st);
                                break;
                            }
                        }
                        _ => {}
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
                    a.clone(),
                );
            }
        }
        _ => {}
    }
}

pub fn do_semantic_tokens(
    id: RequestId,
    uristring: &str,
    lines: &[Rc<Vec<u8>>],
    comments: &HashMap<usize, usize>,
    goto_def: &mut BTreeMap<SemanticTokenSortable, Srcloc>,
    frontend: &CompileForm,
) -> Response {
    let mut collected_tokens = Vec::new();
    let mut varcollection = HashMap::new();
    for form in frontend.helpers.iter() {
        match form {
            HelperForm::Defconstant(defc) => {
                if let Some(kw) = &defc.kw {
                    collected_tokens.push(SemanticTokenSortable {
                        loc: kw.clone(),
                        token_type: TK_KEYWORD_IDX,
                        token_mod: 0,
                    });
                }
                collected_tokens.push(SemanticTokenSortable {
                    loc: defc.nl.clone(),
                    token_type: TK_VARIABLE_IDX,
                    token_mod: (1 << TK_READONLY_BIT) | (1 << TK_DEFINITION_BIT),
                });
                varcollection.insert(defc.name.clone(), defc.nl.clone());
                process_body_code(
                    &mut collected_tokens,
                    goto_def,
                    &HashMap::new(),
                    &varcollection,
                    frontend,
                    defc.body.clone(),
                );
            }
            HelperForm::Defun(_, defun) => {
                let mut argcollection = HashMap::new();
                collected_tokens.push(SemanticTokenSortable {
                    loc: defun.nl.clone(),
                    token_type: TK_FUNCTION_IDX,
                    token_mod: 1 << TK_DEFINITION_BIT,
                });
                if let Some(kw) = &defun.kw {
                    collected_tokens.push(SemanticTokenSortable {
                        loc: kw.clone(),
                        token_type: TK_KEYWORD_IDX,
                        token_mod: 0,
                    });
                }
                collect_arg_tokens(
                    &mut collected_tokens,
                    &mut argcollection,
                    defun.args.clone(),
                );
                process_body_code(
                    &mut collected_tokens,
                    goto_def,
                    &argcollection,
                    &varcollection,
                    frontend,
                    defun.body.clone(),
                );
            }
            HelperForm::Defmacro(mac) => {
                let mut argcollection = HashMap::new();
                collected_tokens.push(SemanticTokenSortable {
                    loc: mac.nl.clone(),
                    token_type: TK_FUNCTION_IDX,
                    token_mod: 1 << TK_DEFINITION_BIT,
                });
                if let Some(kwl) = &mac.kw {
                    collected_tokens.push(SemanticTokenSortable {
                        loc: kwl.clone(),
                        token_type: TK_KEYWORD_IDX,
                        token_mod: 0,
                    });
                }
                collect_arg_tokens(&mut collected_tokens, &mut argcollection, mac.args.clone());
                process_body_code(
                    &mut collected_tokens,
                    goto_def,
                    &argcollection,
                    &varcollection,
                    frontend,
                    mac.program.exp.clone(),
                );
            }
        }
    }

    let mut argcollection = HashMap::new();
    collect_arg_tokens(
        &mut collected_tokens,
        &mut argcollection,
        frontend.args.clone(),
    );
    process_body_code(
        &mut collected_tokens,
        goto_def,
        &argcollection,
        &varcollection,
        frontend,
        frontend.exp.clone(),
    );

    for (l, c) in comments.iter() {
        collected_tokens.push(SemanticTokenSortable {
            loc: DocRange {
                start: DocPosition {
                    line: *l as u32,
                    character: *c as u32,
                },
                end: DocPosition {
                    line: *l as u32,
                    character: lines[*l].len() as u32,
                },
            }
            .to_srcloc(uristring),
            token_type: TK_COMMENT_IDX,
            token_mod: 0,
        });
    }

    collected_tokens.retain(|t| {
        let borrowed: &String = t.loc.file.borrow();
        borrowed == uristring
    });
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
            token_modifiers_bitset: t.token_mod,
        });
        last_row = t.loc.line;
        last_col = t.loc.col;
    }

    Response {
        id,
        error: None,
        result: Some(serde_json::to_value(result_tokens).unwrap()),
    }
}

impl LSPSemtokRequestHandler for LSPServiceProvider {
    fn handle_semantic_tokens(
        &mut self,
        id: RequestId,
        params: &SemanticTokensParams,
    ) -> Result<Vec<Message>, String> {
        let uristring = params.text_document.uri.to_string();
        let mut res = self.parse_document_and_output_errors(&uristring);

        if let (Some(doc), Some(frontend)) = (self.get_doc(&uristring), self.get_parsed(&uristring))
        {
            let mut our_goto_defs = BTreeMap::new();
            let resp = do_semantic_tokens(
                id,
                &uristring,
                &doc.text,
                &doc.comments,
                &mut our_goto_defs,
                &frontend.compiled,
            );
            self.goto_defs.insert(uristring.clone(), our_goto_defs);
            res.push(Message::Response(resp));
        }

        Ok(res)
    }
}
