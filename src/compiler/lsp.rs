use std::cmp::Ordering;
use std::borrow::Borrow;
use std::collections::{HashMap, BTreeMap};
use std::rc::Rc;

use lazy_static::lazy_static;

use lsp_server::ResponseError;
use lsp_types::{
    request::GotoDefinition,
    request::SemanticTokensFullRequest,
    DidOpenTextDocumentParams,
    GotoDefinitionResponse,
    Location,
    Position,
    Range,
    SemanticToken,
    SemanticTokenModifier,
    SemanticTokens,
    SemanticTokenType,
};

use lsp_server::{ExtractError, Message, Request, RequestId, Response};

use crate::compiler::compiler::DefaultCompilerOpts;
use crate::compiler::comptypes::{
    BodyForm, CompileErr, CompilerOpts, CompileForm, HelperForm, LetFormKind
};
use crate::compiler::frontend::frontend;
use crate::compiler::sexp::{SExp, parse_sexp};
use crate::compiler::srcloc::Srcloc;

lazy_static! {
    pub static ref TOKEN_TYPES: Vec<SemanticTokenType> = {
        vec![
            SemanticTokenType::PARAMETER,
            SemanticTokenType::VARIABLE,
            SemanticTokenType::FUNCTION,
            SemanticTokenType::MACRO,
            SemanticTokenType::KEYWORD,
            SemanticTokenType::COMMENT,
            SemanticTokenType::STRING,
            SemanticTokenType::NUMBER,
            SemanticTokenType::OPERATOR
        ]
    };

    pub static ref TOKEN_MODIFIERS: Vec<SemanticTokenModifier> = {
        vec![
            SemanticTokenModifier::DEFINITION,
            SemanticTokenModifier::READONLY,
            SemanticTokenModifier::DOCUMENTATION
        ]
    };
}

pub const TK_PARAMETER_IDX: u32 = 0;
pub const TK_VARIABLE_IDX: u32 = 1;
pub const TK_FUNCTION_IDX: u32 = 2;
pub const TK_MACRO_IDX: u32 = 3;
pub const TK_KEYWORD_IDX: u32 = 4;
pub const TK_COMMENT_IDX: u32 = 5;
pub const TK_STRING_IDX: u32 = 6;
pub const TK_NUMBER_IDX: u32 = 7;
pub const TK_OPERATOR_IDX: u32 = 8;

pub const TK_DEFINITION_BIT: u32 = 0;
pub const TK_READONLY_BIT: u32 = 1;
pub const TK_DOCUMENTATION_BIT: u32 = 2;

fn cast<R>(req: Request) -> Result<(RequestId, R::Params), ExtractError<Request>>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}


#[derive(Debug)]
struct DocData {
    text: String,
}

#[derive(Debug)]
enum ParseResult {
    WithError(CompileErr),
    Completed(CompileForm)
}

#[derive(Debug)]
struct ParsedDoc {
    parses: Vec<u32>,
    result: ParseResult,
}

impl ParsedDoc {
    fn empty() -> Self {
        ParsedDoc {
            parses: vec![],
            result: ParseResult::WithError(CompileErr(Srcloc::start(&"*none*".to_string()), "no file".to_string()))
        }
    }

    fn new(opts: Rc<dyn CompilerOpts>, file: &String, srctext: &String) -> Self {
        let srcloc = Srcloc::start(file);
        parse_sexp(srcloc, srctext).
            map_err(|e| { CompileErr(e.0.clone(), "parse error".to_string()) }).
            map(|parsed| {
                frontend(opts.clone(), &parsed).map(|fe| {
                    ParsedDoc {
                        parses: Vec::new(),
                        result: ParseResult::Completed(fe)
                    }
                }).unwrap_or_else(|e| {
                    ParsedDoc {
                        parses: Vec::new(),
                        result: ParseResult::WithError(e)
                    }
                })
            }).unwrap_or_else(|e| {
                ParsedDoc {
                    parses: Vec::new(),
                    result: ParseResult::WithError(e)
                }
            })
    }
}

fn ensure_parsed_document<'a>(
    document_collection: &HashMap<String, DocData>,
    parsed_documents: &'a mut HashMap<String, ParsedDoc>,
    uristring: &String
) {
    if let Some(doc) = document_collection.get(uristring) {
        let opts = Rc::new(DefaultCompilerOpts::new(uristring));
        let parsed = ParsedDoc::new(opts, uristring, &doc.text);
        parsed_documents.insert(uristring.clone(), parsed);
    }
}

#[derive(Clone, Debug)]
struct SemanticTokenSortable {
    loc: Srcloc,
    token_type: u32,
    token_mod: u32,
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
                                eprintln!("function call at {:?}", st);
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

fn do_semantic_tokens(
    id: RequestId,
    goto_def: &mut BTreeMap<SemanticTokenSortable, Srcloc>,
    frontend: &CompileForm
) -> Response {
    let mut collected_tokens = Vec::new();
    let varcollection = HashMap::new();
    for form in frontend.helpers.iter() {
        match form {
            HelperForm::Defun(_,defun) => {
                let mut argcollection = HashMap::new();
                eprintln!("handling form {}", form.to_sexp().to_string());
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

pub struct LSPServiceProvider {
    document_collection: HashMap<String, DocData>,
    parsed_documents: HashMap<String, ParsedDoc>,
    goto_defs: HashMap<String, BTreeMap<SemanticTokenSortable, Srcloc>>,
}

impl LSPServiceProvider {
    pub fn new() -> Self {
        LSPServiceProvider {
            document_collection: HashMap::new(),
            parsed_documents: HashMap::new(),
            goto_defs: HashMap::new()
        }
    }

    pub fn handle_message(&mut self, msg: &Message) -> Result<Vec<Message>, String> {
        let mut res = Vec::new();
        eprintln!("got msg: {:?}", msg);
        match msg {
            Message::Request(req) => {
                if let Ok((id, params)) = cast::<SemanticTokensFullRequest>(req.clone()) {
                    eprintln!("got semantic token request #{}: for file {}", id, params.text_document.uri.to_string());
                    let uristring = params.text_document.uri.to_string();
                    if self.parsed_documents.get(&uristring).is_none() {
                        eprintln!("ensure parsed");
                        ensure_parsed_document(
                            &self.document_collection,
                            &mut self.parsed_documents,
                            &uristring
                        );
                    }
                    if let Some(parsed) = self.parsed_documents.get(&uristring) {
                        match &parsed.result {
                            ParseResult::Completed(frontend) => {
                                let mut our_goto_defs = BTreeMap::new();
                                let resp = do_semantic_tokens(id, &mut our_goto_defs, &frontend);
                                eprintln!("our goto defs {:?}", our_goto_defs);
                                self.goto_defs.insert(uristring.clone(), our_goto_defs);
                                res.push(Message::Response(resp));
                            },
                            ParseResult::WithError(error) => {
                                let resp = Response { id, result: None, error: Some(ResponseError {
                                    code: 1,
                                    data: None,
                                    message: format!("{}: {}", error.0.to_string(), error.1)
                                }) };
                                res.push(Message::Response(resp));
                            }
                        }
                    } else {
                        eprintln!("no compile output :-(");
                    }
                } else if let Ok((id, params)) = cast::<GotoDefinition>(req.clone()) {
                    eprintln!("got gotoDefinition request #{}: {:?}", id, params);
                    let mut goto_response = None;
                    let docname = params.text_document_position_params.text_document.uri.to_string();
                    let docpos = params.text_document_position_params.position;
                    let wantloc = Srcloc::new(Rc::new(docname.clone()), (docpos.line + 1) as usize, (docpos.character + 1) as usize);
                    if let Some(defs) = self.goto_defs.get(&docname) {
                        eprintln!("find {:?} in {:?}", wantloc, defs);
                        for kv in defs.iter() {
                            if kv.0.loc.overlap(&wantloc) {
                                goto_response = Some(Location {
                                    uri: params.text_document_position_params.text_document.uri.clone(),
                                    range: Range {
                                        start: Position {
                                            line: (kv.1.line - 1) as u32,
                                            character: (kv.1.col - 1) as u32
                                        },
                                        end: Position {
                                            line: (kv.1.line - 1) as u32,
                                            character: (kv.1.col + kv.1.len() - 1) as u32
                                        }
                                    }
                                });
                                break;
                            }
                        }
                    }
                    let result = goto_response.map(|gr| {
                        GotoDefinitionResponse::Scalar(gr)
                    });
                    let result = serde_json::to_value(&result).unwrap();
                    let resp = Response { id, result: Some(result), error: None };
                    res.push(Message::Response(resp));
                } else {
                    eprintln!("unknown request {:?}", req);
                };
                // ...
            }
            Message::Response(resp) => {
                eprintln!("got response: {:?}", resp);
            }
            Message::Notification(not) => {
                eprintln!("got notification: {:?}", not);
                if not.method == "textDocument/didOpen" {
                    let stringified_params = serde_json::to_string(&not.params).unwrap();
                    eprintln!("stringified_params: {}", stringified_params);
                    if let Ok(params) = serde_json::from_str::<DidOpenTextDocumentParams>(&stringified_params) {
                        self.document_collection.insert(params.text_document.uri.to_string(), DocData { text: params.text_document.text.clone() });
                    } else {
                        eprintln!("cast failed in didOpen");
                    }
                } else {
                    eprintln!("not sure what we got: {:?}", not);
                }
            }
        }

        Ok(res)
    }
}
