use std::rc::Rc;

use lsp_types::{
    request::Completion,
    request::GotoDefinition,
    request::SemanticTokensFullRequest,
    DidChangeTextDocumentParams,
    DidOpenTextDocumentParams,
    GotoDefinitionParams,
    GotoDefinitionResponse,
    Location,
    Position,
    Range
};

use lsp_server::{Message, RequestId, Response};

use crate::compiler::lsp::completion::LSPCompletionRequestHandler;
use crate::compiler::lsp::patch::{
    LSPServiceProviderApplyDocumentPatch,
    split_text
};
use crate::compiler::lsp::semtok::LSPSemtokRequestHandler;
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
    fn goto_definition(
        &mut self,
        id: RequestId,
        params: &GotoDefinitionParams
    ) -> Result<Vec<Message>, String> {
        let mut res = Vec::new();

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

        return Ok(res)
    }
}

impl LSPServiceMessageHandler for LSPServiceProvider {
    fn handle_message(&mut self, msg: &Message) -> Result<Vec<Message>, String> {
        eprintln!("got msg: {:?}", msg);
        match msg {
            Message::Request(req) => {
                if let Ok((id, params)) = cast::<SemanticTokensFullRequest>(req.clone()) {
                    return self.handle_semantic_tokens(id, &params);
                } else if let Ok((id, params)) = cast::<GotoDefinition>(req.clone()) {
                    return self.goto_definition(id, &params);
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

        Ok(vec![])
    }
}
