use serde_json::{Value, to_value, from_str};
use lsp_server::{
    Message,
    Notification,
    Request,
    RequestId,
    Response
};
use lsp_types::{
    DidOpenTextDocumentParams,
    PartialResultParams,
    SemanticToken,
    SemanticTokens,
    SemanticTokensParams,
    TextDocumentIdentifier,
    TextDocumentItem,
    Url,
    WorkDoneProgressParams,
};
use crate::compiler::lsp::{
    LSPServiceProvider,
    TK_FUNCTION_IDX,
    TK_DEFINITION_BIT
};

fn make_did_open_message(uri: &String, v: i32, body: String) -> Message {
    Message::Notification(Notification {
        method: "textDocument/didOpen".to_string(),
        params: serde_json::to_value(DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: Url::parse(uri).unwrap(),
                language_id: "chialisp".to_string(),
                version: v,
                text: body
            }
        }).unwrap()
    })
}

fn make_get_semantic_tokens_msg(uri: &String, rid: i32) -> Message {
    Message::Request(Request {
        id: RequestId::from(rid),
        method: "textDocument/semanticTokens/full".to_string(),
        params: serde_json::to_value(SemanticTokensParams {
            work_done_progress_params: WorkDoneProgressParams {
                work_done_token: None
            },
            partial_result_params: PartialResultParams {
                partial_result_token: None
            },
            text_document: TextDocumentIdentifier { uri: Url::parse(uri).unwrap() }
        }).unwrap()
    })
}

fn get_msg_params(msg: &Message) -> String {
    match msg {
        Message::Request(req) => req.params.to_string(),
        Message::Notification(not) => not.params.to_string(),
        Message::Response(res) => res.result.as_ref().map(|r| serde_json::to_string(r).unwrap()).unwrap_or_else(|| "null".to_string())
    }
}

#[test]
fn smoke() {
    let lsp = LSPServiceProvider::new();
}

#[test]
fn can_receive_did_open_file_and_give_semantic_tokens() {
    let mut lsp = LSPServiceProvider::new();
    let file = "file:test.cl".to_string();
    let open_msg = make_did_open_message(&file, 1, "(mod () (defun F () ()) (F))".to_string());
    let sem_tok = make_get_semantic_tokens_msg(&file, 2);
    lsp.handle_message(&open_msg).expect("should be ok to take open msg");
    let r2 = lsp.handle_message(&sem_tok).expect("should be ok to send sem tok");
    let decoded_tokens: SemanticTokens = serde_json::from_str(&get_msg_params(&r2[0])).unwrap();
    assert_eq!(
        decoded_tokens.data,
        vec![
            SemanticToken {
                delta_line: 0,
                delta_start: 15,
                length: 1,
                token_type: TK_FUNCTION_IDX,
                token_modifiers_bitset: 1 << TK_DEFINITION_BIT
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 10,
                length: 1,
                token_type: TK_FUNCTION_IDX,
                token_modifiers_bitset: 0
            }
        ]
    );
}
