use std::cell::{Ref, RefCell};
use std::cmp::Ordering;
use std::borrow::Borrow;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::mem::swap;
use std::rc::Rc;

use lazy_static::lazy_static;

use lsp_server::ResponseError;
use lsp_types::{
    request::Completion,
    request::GotoDefinition,
    request::SemanticTokensFullRequest,
    CompletionItem,
    CompletionList,
    CompletionParams,
    CompletionResponse,
    DidChangeTextDocumentParams,
    DidOpenTextDocumentParams,
    GotoDefinitionResponse,
    Location,
    Position,
    Range,
    SemanticToken,
    SemanticTokenModifier,
    SemanticTokens,
    SemanticTokenType,
    TextDocumentContentChangeEvent
};

use lsp_server::{ExtractError, Message, Request, RequestId, Response};

use crate::compiler::comptypes::{
    BodyForm, CompileErr, CompilerOpts, CompileForm, HelperForm, LetFormKind
};
use crate::compiler::frontend::frontend;
use crate::compiler::lsp::compopts::LSPCompilerOpts;
use crate::compiler::sexp::{SExp, parse_sexp, decode_string};
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

#[derive(Debug, Clone)]
pub enum ScopeKind {
    Module,
    Macro,
    Function,
    Let
}

#[derive(Debug, Clone)]
pub struct DocData {
    pub text: Vec<Rc<Vec<u8>>>,
}

#[derive(Debug, Clone)]
pub struct ParseScope {
    region: Srcloc,
    kind: ScopeKind,
    variables: HashSet<String>,
    functions: HashSet<String>,
    containing: Vec<ParseScope>
}

#[derive(Debug, Clone)]
pub struct ParseOutput {
    compiled: CompileForm,
    scopes: ParseScope
}

#[derive(Debug, Clone)]
pub enum ParseResult {
    WithError(CompileErr),
    Completed(ParseOutput)
}

#[derive(Debug, Clone)]
pub struct ParsedDoc {
    parses: Vec<u32>,
    result: ParseResult,
}

pub struct DocVecByteIter<'a> {
    line: usize,
    offs: usize,
    target: &'a [Rc<Vec<u8>>]
}

impl<'a> Iterator for DocVecByteIter<'a> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.line >= self.target.len() {
                return None;
            } else if self.offs >= self.target[self.line].len() {
                self.line += 1;
                self.offs = 0;
                return Some(b'\n');
            } else {
                let res = self.target[self.line][self.offs];
                self.offs += 1;
                return Some(res);
            }
        }
    }
}

impl<'a> DocVecByteIter<'a> {
    fn new(target: &'a [Rc<Vec<u8>>]) -> Self {
        DocVecByteIter {
            line: 0,
            offs: 0,
            target
        }
    }
}

fn grab_scope_range(text: &[Rc<Vec<u8>>], loc: Srcloc) -> String {
    let eloc = loc.ending();
    let mut res = Vec::new();

    if (loc.line - 1) >= text.len() {
        return "".to_string();
    }

    // First line
    let tline = text[loc.line - 1].clone();
    let text_borrowed: &Vec<u8> = tline.borrow();

    if eloc.line == loc.line {
        // Only line
        for ch in text_borrowed.iter().take(eloc.col).skip(loc.col - 1) {
            res.push(*ch);
        }
    } else {
        for ch in text_borrowed.iter().skip(loc.col - 1) {
            res.push(*ch);
        }
    }

    for ch in text_borrowed.iter() {
        res.push(*ch);
    }

    // Inside lines.
    let end_line =
        if eloc.line - 1 > text.len() {
            text.len()
        } else {
            eloc.line - 1
        };

    eprintln!("loc.line {} end_line {}", loc.line, end_line);
    if loc.line < end_line - 1 {
        for l in text[loc.line..end_line - 1].iter() {
            res.push(b'\n');
            let iline = l.clone();
            let il: &Vec<u8> = iline.borrow();
            for ch in il.iter() {
                res.push(*ch);
            }
        }
    }
    
    let eline = text[end_line].clone();
    let end_borrowed: &Vec<u8> = eline.borrow();

    res.push(b'\n');
    for ch in end_borrowed.iter().take(eloc.col - 1) {
        res.push(*ch);
    }

    decode_string(&res)
}

fn make_arg_set(set: &mut HashSet<String>, args: Rc<SExp>) {
    match args.borrow() {
        SExp::Atom(l,a) => {
            set.insert(decode_string(&a));
        },
        SExp::Cons(l,a,b) => {
            make_arg_set(set, a.clone());
            make_arg_set(set, b.clone());
        },
        _ => { }
    }
}

fn make_helper_scope(h: &HelperForm) -> Option<ParseScope> {
    let loc = h.loc();
    let eloc = loc.ending();

    let mut kind = None;
    let mut args = HashSet::new();

    match h {
        HelperForm::Defun(i,d) => {
            kind = Some(ScopeKind::Function);
            make_arg_set(&mut args, d.args.clone());
        },
        HelperForm::Defmacro(m) => {
            kind = Some(ScopeKind::Macro);
            make_arg_set(&mut args, m.args.clone());
        },
        _ => { }
    }

    kind.map(|k| {
        ParseScope {
            kind: k,
            region: loc,
            variables: args,
            functions: HashSet::new(),
            containing: Vec::new()
        }
    })
}

fn recover_scopes(ourfile: &String, text: &[Rc<Vec<u8>>], fe: &CompileForm) -> ParseScope {
    let mut toplevel_args = HashSet::new();
    let mut toplevel_funs = HashSet::new();
    let mut contained = Vec::new();

    make_arg_set(&mut toplevel_args, fe.args.clone());

    for h in fe.helpers.iter() {
        eprintln!("helper {} has this range in the code: {}", h.to_sexp().to_string(), grab_scope_range(text, h.loc()));

        match h {
            HelperForm::Defun(i,d) => {
                toplevel_funs.insert(decode_string(&d.name));
            },
            HelperForm::Defmacro(m) => {
                toplevel_funs.insert(decode_string(&m.name));
            },
            HelperForm::Defconstant(l,n,c) => {
                toplevel_args.insert(decode_string(&n));
            }
        }

        let f = h.loc().file.clone();
        let filename: &String = f.borrow();
        if filename == ourfile {
            if let Some(scope) = make_helper_scope(h) {
                contained.push(scope);
            }
        }
    }

    ParseScope {
        kind: ScopeKind::Module,
        region: Srcloc::start(ourfile).ext(
            &Srcloc::new(Rc::new(ourfile.clone()), text.len() + 1, 0)
        ),
        variables: toplevel_args,
        functions: toplevel_funs,
        containing: contained
    }
}

impl ParsedDoc {
    fn empty() -> Self {
        ParsedDoc {
            parses: vec![],
            result: ParseResult::WithError(CompileErr(Srcloc::start(&"*none*".to_string()), "no file".to_string()))
        }
    }

    fn new(opts: Rc<dyn CompilerOpts>, file: &String, srctext: &[Rc<Vec<u8>>]) -> Self {
        let srcloc = Srcloc::start(file);
        parse_sexp(srcloc, DocVecByteIter::new(srctext)).
            map_err(|e| { CompileErr(e.0.clone(), "parse error".to_string()) }).
            map(|parsed| {
                frontend(opts.clone(), &parsed).as_ref().map(|fe| {
                    let parsed = ParseOutput {
                        compiled: fe.clone(),
                        // Todo: fill out
                        scopes: recover_scopes(file, srctext, fe)
                    };
                    ParsedDoc {
                        parses: Vec::new(),
                        result: ParseResult::Completed(parsed)
                    }
                }).unwrap_or_else(|e| {
                    ParsedDoc {
                        parses: Vec::new(),
                        result: ParseResult::WithError(e.clone())
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

pub struct LSPServiceProvider {
    document_collection: Rc<RefCell<HashMap<String, DocData>>>,
    parsed_documents: Rc<RefCell<HashMap<String, ParsedDoc>>>,
    goto_defs: HashMap<String, BTreeMap<SemanticTokenSortable, Srcloc>>,
}

fn split_text(td: &String) -> Vec<Rc<Vec<u8>>> {
    let result: Vec<Rc<Vec<u8>>> = td.split("\n").map(|x| Rc::new(x.as_bytes().to_vec())).collect();
    result
}

pub fn stringify_doc(d: &Vec<Rc<Vec<u8>>>) -> Result<String, String> {
    let bytes = DocVecByteIter::new(d).collect();
    String::from_utf8(bytes).map_err(|_| "no conversion from utf8".to_string())
}

fn find_ident(line: Rc<Vec<u8>>, char_at: u32) -> Option<Vec<u8>> {
    let ca_size = char_at as usize;
    let ll = line.len();

    if ca_size > ll {
        return None;
    }

    let borrowed: &Vec<u8> = line.borrow();
    let mut lb = ca_size - 1;
    let mut ub = ca_size;
    while lb > 0 && borrowed[lb-1].is_ascii_alphabetic() {
        lb -= 1;
    }
    while ub < borrowed.len() && borrowed[ub].is_ascii_alphanumeric() {
        ub += 1;
    }

    let ident_vec: Vec<u8> =
        borrowed[lb..].iter().
        take(ub - lb).copied().collect();

    Some(ident_vec)
}

fn find_scope_stack(
    out_scopes: &mut Vec<ParseScope>,
    scope: &ParseScope,
    position: &Srcloc
) {
    if scope.region.overlap(&position) {
        for s in scope.containing.iter() {
            find_scope_stack(out_scopes, s, position);
        }
        out_scopes.push(scope.clone());
    }
}

impl LSPServiceProvider {
    fn get_doc(&self, uristring: &String) -> Option<DocData> {
        let cell: &RefCell<HashMap<String, DocData>> = self.document_collection.borrow();
        let coll: Ref<HashMap<String, DocData>> = cell.borrow();
        (&coll).get(uristring).map(|x| x.clone())
    }

    pub fn get_parsed(&self, uristring: &String) -> Option<ParsedDoc> {
        let cell: &RefCell<HashMap<String, ParsedDoc>> = self.parsed_documents.borrow();
        let coll: Ref<HashMap<String, ParsedDoc>> = cell.borrow();
        (&coll).get(uristring).cloned()
    }

    fn save_doc(&self, uristring: String, dd: DocData) {
        let cell: &RefCell<HashMap<String, DocData>> = self.document_collection.borrow();
        cell.replace_with(|coll| {
            let mut repl = HashMap::new();
            swap(&mut repl, coll);
            repl.insert(uristring.clone(), dd);
            repl
        });
        let pcell: &RefCell<HashMap<String, ParsedDoc>> = self.parsed_documents.borrow();
        pcell.replace_with(|pcoll| {
            let mut repl = HashMap::new();
            swap(&mut repl, pcoll);
            repl.remove(&uristring);
            repl
        });
    }

    fn save_parse(&self, uristring: String, p: ParsedDoc) {
        let cell: &RefCell<HashMap<String, ParsedDoc>> = self.parsed_documents.borrow();
        cell.replace_with(|pcoll| {
            let mut repl = HashMap::new();
            swap(&mut repl, pcoll);
            repl.insert(uristring, p);
            repl
        });
    }

    fn get_positional_text(&mut self, uri: &String, position: &Position) -> Option<Vec<u8>> {
        eprintln!("get positional text: {:?}", position);
        self.get_doc(uri).and_then(|lines| {
            let pl = position.line as usize;
            if pl < lines.text.len() {
                Some(lines.text[pl].clone())
            } else {
                None
            }
        }).and_then(|line| {
            if position.character == 0 {
                None
            } else {
                find_ident(line.clone(), position.character)
            }
        })
    }

    fn apply_document_patch(&mut self, uristring: &String, patches: &Vec<TextDocumentContentChangeEvent>) {
        if let Some(dd) = self.get_doc(uristring) {
            let mut last_line = 1;
            let mut last_col = 1;

            if patches.len() == 1 && patches[0].range.is_none() {
                // We can short circuit a full document rewrite.
                self.save_doc(uristring.clone(), DocData {
                    text: split_text(&patches[0].text)
                });
                return;
            }

            // Try to do an efficient job of patching the old document content.
            let mut doc_copy = dd.text.clone();
            for p in patches.iter() {
                if let Some(r) = p.range {
                    let split_data = split_text(&p.text);
                    let replaced_line = 0;
                    let rstart_line = r.start.line as usize;
                    let rend_line = r.end.line as usize;
                    let rstart_char = r.start.character as usize;
                    let rend_char = r.end.character as usize;
                    let original_end_line = rend_line;
                    for (i, l) in split_data.iter().enumerate() {
                        let match_line = rstart_line + i;
                        let begin_this_line =
                            if rstart_line == i {
                                rstart_char
                            } else {
                                0
                            };
                        let end_this_line =
                            if match_line == rend_line {
                                rend_char
                            } else {
                                doc_copy[match_line].len()
                            };
                        let (prefix, mut suffix) =
                            if match_line == rstart_line {
                                ((&doc_copy[match_line])[0..begin_this_line].to_vec(), Vec::new())
                            } else if match_line == rend_line {
                                (Vec::new(), (&doc_copy[match_line])[end_this_line..].to_vec())
                            } else {
                                (Vec::new(), Vec::new())
                            };
                        let new_line =
                            if prefix.is_empty() && suffix.is_empty() {
                                // Whole line
                                l.clone()
                            } else {
                                // Partial line
                                let mut vec_copy = prefix.clone();
                                let line_borrowed: &Vec<u8> = l.borrow();
                                vec_copy.append(&mut line_borrowed.clone());
                                vec_copy.append(&mut suffix);
                                Rc::new(vec_copy)
                            };

                        if match_line >= original_end_line {
                            // Insert the new line
                            doc_copy.insert(match_line, new_line);
                        } else {
                            // Overwrite line
                            doc_copy[match_line] = new_line;
                        }
                    }
                } else {
                    doc_copy = split_text(&p.text)
                }
            }

            eprintln!("apply document patch: {}", stringify_doc(&doc_copy).unwrap_or_else(|_| "*error*".to_string()));
            self.save_doc(uristring.clone(), DocData {
                text: doc_copy
            });
        }
    }

    fn ensure_parsed_document<'a>(
        &mut self,
        uristring: &String
    ) {
        if let Some(doc) = self.get_doc(uristring) {
            let opts = Rc::new(LSPCompilerOpts::new(uristring.clone(), self.document_collection.clone()));
            let parsed = ParsedDoc::new(opts, uristring, &doc.text);
            self.save_parse(uristring.clone(), parsed);
        }
    }

    pub fn new() -> Self {
        LSPServiceProvider {
            document_collection: Rc::new(RefCell::new(HashMap::new())),
            parsed_documents: Rc::new(RefCell::new(HashMap::new())),
            goto_defs: HashMap::new()
        }
    }

    pub fn get_file(&self, filename: &String) -> Result<String, String> {
        self.get_doc(filename).map(|d| stringify_doc(&d.text)).unwrap_or_else(|| Err(format!("don't have file {}", filename)))
    }

    // Completions:
    //
    // Functions:
    //
    // - Top level: show top level functions
    // - Inside macro: Show functions at this level
    // - Inside function: Show top level functions
    //
    // Variables:
    //
    // - Top level: show top level variables
    // - Inside macro: Show variables at this level
    // - Inside function: Show top level constants and
    //   variables at this level.
    //
    // If at the head of a list: show function completions
    // Otherwise show variable completions.
    pub fn handle_completion_request(&mut self, id: RequestId, params: &CompletionParams) -> Result<Vec<Message>, String> {
        let mut res = Vec::new();
        let uristring =
            params.text_document_position.text_document.uri.to_string();
        self.ensure_parsed_document(&uristring);

        if let (Some(cpl), Some(parsed)) = (
            self.get_positional_text(&uristring, &params.text_document_position.position),
            self.get_parsed(&uristring)
        ) {
            if let ParseResult::Completed(output) = parsed.result {
                let mut found_scopes = Vec::new();
                let pos = params.text_document_position.position;
                let want_position = Srcloc::new(
                    Rc::new(uristring.clone()),
                    (pos.line + 1) as usize,
                    (pos.character + 1) as usize
                );
                find_scope_stack(
                    &mut found_scopes,
                    &output.scopes,
                    &want_position
                );
                eprintln!("scopes: {:?}", found_scopes);
                let mut result_items = Vec::new();
                if found_scopes.len() == 1 {
                } else {
                    for s in found_scopes.iter() {
                        for sym in s.variables.iter() {
                            result_items.push(CompletionItem {
                                label: sym.clone(),
                                ..Default::default()
                            });
                        }
                    }
                }
                let result = CompletionResponse::List(CompletionList {
                    is_incomplete: false,
                    items: result_items
                });
                let result = serde_json::to_value(&result).unwrap();
                let resp = Response { id, result: Some(result), error: None };
                res.push(Message::Response(resp));
            }
        }

        Ok(res)
    }

    pub fn handle_message(&mut self, msg: &Message) -> Result<Vec<Message>, String> {
        let mut res = Vec::new();
        eprintln!("got msg: {:?}", msg);
        match msg {
            Message::Request(req) => {
                if let Ok((id, params)) = cast::<SemanticTokensFullRequest>(req.clone()) {
                    eprintln!("got semantic token request #{}: for file {}", id, params.text_document.uri.to_string());
                    let uristring = params.text_document.uri.to_string();

                    self.ensure_parsed_document(&uristring);

                    if let Some(parsed) = self.get_parsed(&uristring) {
                        match &parsed.result {
                            ParseResult::Completed(frontend) => {
                                let mut our_goto_defs = BTreeMap::new();
                                let resp = do_semantic_tokens(id, &uristring, &mut our_goto_defs, &frontend.compiled);
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
                } else if let Ok((id, params)) = cast::<Completion>(req.clone()) {
                    self.handle_completion_request(id, &params);
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
                    if let Ok(params) = serde_json::from_str::<DidOpenTextDocumentParams>(&stringified_params) {
                        self.save_doc(
                            params.text_document.uri.to_string(),
                            DocData { text: split_text(&params.text_document.text) }
                        );
                    } else {
                        eprintln!("cast failed in didOpen");
                    }
                } else if not.method == "textDocument/didChange" {
                    let stringified_params = serde_json::to_string(&not.params).unwrap();
                    if let Ok(params) = serde_json::from_str::<DidChangeTextDocumentParams>(&stringified_params) {
                        let doc_id = params.text_document.uri.to_string();
                        self.apply_document_patch(&doc_id, &params.content_changes);
                    } else {
                        eprintln!("case failed in didChange");
                    }
                } else {
                    eprintln!("not sure what we got: {:?}", not);
                }
            }
        }

        Ok(res)
    }
}
