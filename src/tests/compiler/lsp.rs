use crate::compiler::lsp::{
    LSPServiceProvider,
    TK_FUNCTION_IDX,
    TK_DEFINITION_BIT
};
use crate::compiler::comptypes::{CompileForm, CompileErr};
use crate::compiler::frontend::frontend;

use serde_json::{Value, to_value, from_str};
use lsp_server::{
    Message,
    Notification,
    Request,
    RequestId,
    Response
};
use lsp_types::{
    CompletionItem,
    CompletionParams,
    CompletionResponse,
    DidOpenTextDocumentParams,
    PartialResultParams,
    Position,
    SemanticToken,
    SemanticTokens,
    SemanticTokensParams,
    TextDocumentIdentifier,
    TextDocumentItem,
    TextDocumentPositionParams,
    Url,
    WorkDoneProgressParams,
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

fn make_completion_request_msg(uri: &String, rid: i32, position: Position) -> Message {
    Message::Request(Request {
        id: RequestId::from(rid),
        method: "textDocument/completion".to_string(),
        params: serde_json::to_value(CompletionParams {
            text_document_position: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri: Url::parse(uri).unwrap() },
                position: position
            },
            work_done_progress_params: WorkDoneProgressParams {
                work_done_token: None,
            },
            partial_result_params: PartialResultParams {
                partial_result_token: None,
            },
            context: None
        }).unwrap()
    })
}

fn decode_completion_response(m: &Message) -> Option<Vec<CompletionItem>> {
    serde_json::from_str(&serde_json::to_value(&m).unwrap().to_string()).ok().and_then(|deser| {
        if let Message::Response(cr) = deser {
            Some(cr)
        } else {
            None
        }
    }).and_then(|cr| cr.result).and_then(|cr| {
        serde_json::from_str(&cr.to_string()).ok()
    }).map(|cr| {
        match cr {
            CompletionResponse::Array(v) => v.clone(),
            CompletionResponse::List(cl) => cl.items.clone()
        }
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

// Run an lsp over some messages so we can check out what it does.
fn run_lsp(
    lsp: &mut LSPServiceProvider,
    messages: &Vec<Message>
) -> Result<Vec<Message>, String> {
    let mut res = Vec::new();
    for m in messages.iter() {
        res.append(&mut lsp.handle_message(m)?);
    }
    Ok(res)
}

#[test]
fn test_completion() {
    let mut lsp = LSPServiceProvider::new();
    let file = "file:test.cl".to_string();
    let open_msg = make_did_open_message(&file, 1, indoc!{"
(mod (A) ;;; COLLATZ conjecture

;; set language standard
  (include *standard-cl-22*)
;; Determine if number is odd
  (defun-inline odd (X) (logand X 1))
                ;; Actual collatz function
  ;; determines number of step til 1
  (defun collatz (N X)
    (if (= X 1) ; We got 1
      N ; Return the number of steps
      (let ((incN (+ N 1))) ; Next N
        (if (odd X) ; Is it odd?
          (collatz in (+ 1 (* 3 X))) ; Odd? 3 X + 1
          (collatz incN (/ X 2)) ; Even? X / 2
          )
        )
      )
    )
  (collatz 0 A) ; Run it
  )            "}.to_string());
    let complete_msg = make_completion_request_msg(
        &file, 2, Position { line: 13, character: 21 }
    );
    let out_msgs = run_lsp(&mut lsp, &vec![open_msg, complete_msg]).unwrap();
    let parsed = lsp.get_parsed(&file).unwrap();
    eprintln!("parsed {:?}", parsed);
    assert_eq!(out_msgs.len() > 0, true);
    let completion_result = decode_completion_response(&out_msgs[0]).unwrap();
    assert_eq!(completion_result.len() > 0, true);
}
