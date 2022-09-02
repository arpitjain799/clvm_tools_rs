use std::cell::{Ref, RefCell};
use std::borrow::Borrow;
use std::collections::{BTreeMap, HashMap};
use std::mem::swap;
use std::rc::Rc;

use lsp_server::ResponseError;
use lsp_types::{
    request::Completion,
    request::GotoDefinition,
    request::SemanticTokensFullRequest,
    DidChangeTextDocumentParams,
    DidOpenTextDocumentParams,
    GotoDefinitionResponse,
    Location,
    Position,
    Range
};

use lsp_server::{Message, Response};

use crate::compiler::lsp::completion::LSPCompletionRequestHandler;
use crate::compiler::lsp::compopts::LSPCompilerOpts;
use crate::compiler::lsp::parse::{
    ParsedDoc,
    ParseOutput,
    ParseResult
};
use crate::compiler::lsp::patch::{
    LSPServiceProviderApplyDocumentPatch,
    split_text,
    stringify_doc
};
use crate::compiler::lsp::semtok::do_semantic_tokens;
use crate::compiler::lsp::types::{
    cast,
    DocData,
    LSPServiceProvider
};
use crate::compiler::srcloc::Srcloc;

pub trait LSPServiceMessageHandler {
    fn handle_message(&mut self, msg: &Message) -> Result<Vec<Message>, String>;
}

impl LSPServiceProvider {
    pub fn with_doc_and_parsed<F,G>(&mut self, uristring: &String, f: F) -> Option<G>
    where
        F: FnOnce(&DocData, &ParseOutput) -> Option<G>
    {
        self.ensure_parsed_document(uristring);
        if let (Some(d), Some(p)) = (self.get_doc(uristring), self.get_parsed(uristring)) {
            if let ParseResult::Completed(o) = p.result {
                f(&d, &o)
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn get_doc(&self, uristring: &String) -> Option<DocData> {
        let cell: &RefCell<HashMap<String, DocData>> = self.document_collection.borrow();
        let coll: Ref<HashMap<String, DocData>> = cell.borrow();
        (&coll).get(uristring).map(|x| x.clone())
    }

    pub fn get_parsed(&self, uristring: &String) -> Option<ParsedDoc> {
        let cell: &RefCell<HashMap<String, ParsedDoc>> = self.parsed_documents.borrow();
        let coll: Ref<HashMap<String, ParsedDoc>> = cell.borrow();
        coll.get(uristring).cloned()
    }

    pub fn save_doc(&self, uristring: String, dd: DocData) {
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
}

impl LSPServiceMessageHandler for LSPServiceProvider {
    fn handle_message(&mut self, msg: &Message) -> Result<Vec<Message>, String> {
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
                    return self.handle_completion_request(id, &params);
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
