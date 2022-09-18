use regex::Regex;
use std::rc::Rc;

use crate::compiler::lsp::{
    LSPServiceMessageHandler, LSPServiceProvider, TK_DEFINITION_BIT, TK_FUNCTION_IDX,
};

use lsp_server::{Message, Notification, Request, RequestId};
use lsp_types::{
    CompletionItem, CompletionParams, CompletionResponse, DidOpenTextDocumentParams,
    PartialResultParams, Position, Range, SemanticToken, SemanticTokens, SemanticTokensParams,
    TextDocumentContentChangeEvent, TextDocumentIdentifier, TextDocumentItem,
    TextDocumentPositionParams, Url, WorkDoneProgressParams,
};

use crate::compiler::compiler::DefaultCompilerOpts;
use crate::compiler::comptypes::CompilerOpts;
use crate::compiler::lsp::parse::{is_first_in_list, make_simple_ranges, ParsedDoc};
use crate::compiler::lsp::patch::{split_text, stringify_doc, PatchableDocument};
use crate::compiler::lsp::reparse::{combine_new_with_old_parse, reparse_subset};
use crate::compiler::lsp::types::{DocData, DocPosition, DocRange, EPrintWriter, FSFileReader};
use crate::compiler::srcloc::Srcloc;

fn make_did_open_message(uri: &String, v: i32, body: String) -> Message {
    Message::Notification(Notification {
        method: "textDocument/didOpen".to_string(),
        params: serde_json::to_value(DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: Url::parse(uri).unwrap(),
                language_id: "chialisp".to_string(),
                version: v,
                text: body,
            },
        })
        .unwrap(),
    })
}

fn make_get_semantic_tokens_msg(uri: &String, rid: i32) -> Message {
    Message::Request(Request {
        id: RequestId::from(rid),
        method: "textDocument/semanticTokens/full".to_string(),
        params: serde_json::to_value(SemanticTokensParams {
            work_done_progress_params: WorkDoneProgressParams {
                work_done_token: None,
            },
            partial_result_params: PartialResultParams {
                partial_result_token: None,
            },
            text_document: TextDocumentIdentifier {
                uri: Url::parse(uri).unwrap(),
            },
        })
        .unwrap(),
    })
}

fn make_completion_request_msg(uri: &String, rid: i32, position: Position) -> Message {
    Message::Request(Request {
        id: RequestId::from(rid),
        method: "textDocument/completion".to_string(),
        params: serde_json::to_value(CompletionParams {
            text_document_position: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier {
                    uri: Url::parse(uri).unwrap(),
                },
                position: position,
            },
            work_done_progress_params: WorkDoneProgressParams {
                work_done_token: None,
            },
            partial_result_params: PartialResultParams {
                partial_result_token: None,
            },
            context: None,
        })
        .unwrap(),
    })
}

fn decode_completion_response(m: &Message) -> Option<Vec<CompletionItem>> {
    serde_json::from_str(&serde_json::to_value(&m).unwrap().to_string())
        .ok()
        .and_then(|deser| {
            if let Message::Response(cr) = deser {
                Some(cr)
            } else {
                None
            }
        })
        .and_then(|cr| cr.result)
        .and_then(|cr| serde_json::from_str(&cr.to_string()).ok())
        .map(|cr| match cr {
            CompletionResponse::Array(v) => v.clone(),
            CompletionResponse::List(cl) => cl.items.clone(),
        })
}

fn get_msg_params(msg: &Message) -> String {
    match msg {
        Message::Request(req) => req.params.to_string(),
        Message::Notification(not) => not.params.to_string(),
        Message::Response(res) => res
            .result
            .as_ref()
            .map(|r| serde_json::to_string(r).unwrap())
            .unwrap_or_else(|| "null".to_string()),
    }
}

#[test]
fn can_receive_did_open_file_and_give_semantic_tokens() {
    let mut lsp = LSPServiceProvider::new(
        Rc::new(FSFileReader::new()),
        Rc::new(EPrintWriter::new()),
        true,
    );
    let file = "file:test.cl".to_string();
    let open_msg = make_did_open_message(&file, 1, "(mod () (defun F () ()) (F))".to_string());
    let sem_tok = make_get_semantic_tokens_msg(&file, 2);
    lsp.handle_message(&open_msg)
        .expect("should be ok to take open msg");
    let r2 = lsp
        .handle_message(&sem_tok)
        .expect("should be ok to send sem tok");
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
fn run_lsp(lsp: &mut LSPServiceProvider, messages: &Vec<Message>) -> Result<Vec<Message>, String> {
    let mut res = Vec::new();
    for m in messages.iter() {
        let mut new_msgs = lsp.handle_message(m)?;
        res.append(&mut new_msgs);
    }
    Ok(res)
}

#[test]
fn test_completion_from_argument_single_level() {
    let mut lsp = LSPServiceProvider::new(
        Rc::new(FSFileReader::new()),
        Rc::new(EPrintWriter::new()),
        true,
    );
    let file = "file:///test.cl".to_string();
    let open_msg = make_did_open_message(
        &file,
        1,
        indoc! {"
(mod (A) ;;; COLLATZ conjecture

;; set language standard
  (include *standard-cl-22*)
;; Determine if number is odd
  (defun-inline odd (X) (logand X 1))
                ;; Actual collatz function
  ;; determines number of step til 1
  (defun collatz (N X zoom)
    (if (= X 1) ; We got 1
      N ; Return the number of steps
      (let ((incN (+ N 1))) ; Next N
        (if (odd X) ; Is it odd?
          (collatz zoo (+ 1 (* 3 X))) ; Odd? 3 X + 1
          (collatz incN (/ X 2)) ; Even? X / 2
          )
        )
      )
    )
  (collatz 0 A) ; Run it
  )            "}
        .to_string(),
    );
    let complete_msg = make_completion_request_msg(
        &file,
        2,
        Position {
            line: 13,
            character: 21,
        },
    );
    let out_msgs = run_lsp(&mut lsp, &vec![open_msg, complete_msg]).unwrap();
    assert_eq!(out_msgs.len() > 0, true);
    let completion_result = decode_completion_response(&out_msgs[0]).unwrap();
    assert_eq!(completion_result.len() > 0, true);
    assert_eq!(completion_result[0].label, "zoom");
}

#[test]
fn test_completion_from_argument_top_level_only_expected() {
    let mut lsp = LSPServiceProvider::new(
        Rc::new(FSFileReader::new()),
        Rc::new(EPrintWriter::new()),
        true,
    );
    let file = "file:///test.cl".to_string();
    let open_msg = make_did_open_message(
        &file,
        1,
        indoc! {"
(mod (X1) ;;; COLLATZ conjecture

;; set language standard
  (include *standard-cl-22*)
;; Determine if number is odd
  (defun-inline odd (X2) (logand X2 1))
                ;; Actual collatz function
  ;; determines number of step til 1
  (defun collatz (N X3)
    (if (= X3 1) ; We got 1
      N ; Return the number of steps
      (let ((incN (+ N 1))) ; Next N
        (if (odd X3) ; Is it odd?
          (collatz zoo (+ 1 (* 3 X3))) ; Odd? 3 X + 1
          (collatz incN (/ X3 2)) ; Even? X / 2
          )
        )
      )
    )
  (collatz 0 X) ; Run it
  )            "}
        .to_string(),
    );
    let complete_msg = make_completion_request_msg(
        &file,
        2,
        Position {
            line: 19,
            character: 14,
        },
    );
    let out_msgs = run_lsp(&mut lsp, &vec![open_msg, complete_msg]).unwrap();
    assert_eq!(out_msgs.len() > 0, true);
    let completion_result = decode_completion_response(&out_msgs[0]).unwrap();
    assert_eq!(completion_result.len() == 1, true);
    assert_eq!(completion_result[0].label, "X1");
}

#[test]
fn test_completion_from_argument_function() {
    let mut lsp = LSPServiceProvider::new(
        Rc::new(FSFileReader::new()),
        Rc::new(EPrintWriter::new()),
        true,
    );
    let file = "file:///test.cl".to_string();
    let open_msg = make_did_open_message(
        &file,
        1,
        indoc! {"
(mod (A) ;;; COLLATZ conjecture
  (defun-inline odd (X) (logand X 1))
  (+ (od) 2)
  )"}
        .to_string(),
    );
    let complete_msg = make_completion_request_msg(
        &file,
        2,
        Position {
            line: 2,
            character: 8,
        },
    );
    let out_msgs = run_lsp(&mut lsp, &vec![open_msg, complete_msg]).unwrap();
    let mut completion_responses = Vec::new();
    for c in out_msgs.iter() {
        if let Some(completion_result) = decode_completion_response(&c) {
            completion_responses.push(completion_result[0].clone());
        }
    }
    assert_eq!(completion_responses.len(), 1);
    assert_eq!(completion_responses[0].label, "odd");
}

#[test]
fn test_first_in_list() {
    let file_data = "( test1 test2)".to_string();
    let doc = DocData {
        text: split_text(&file_data),
        version: -1,
    };
    let pos = Position {
        line: 0,
        character: 5,
    };
    assert_eq!(is_first_in_list(&doc, &pos), true);
}

#[test]
fn test_not_first_in_list() {
    let file_data = "( test1 test2)".to_string();
    let doc = DocData {
        text: split_text(&file_data),
        version: -1,
    };
    let pos = Position {
        line: 0,
        character: 10,
    };
    assert_eq!(is_first_in_list(&doc, &pos), false);
}

#[test]
fn test_patch_document_1() {
    let content = "(mod (A) ;;; COLLATZ conjecture\n\n;; set language standard\n  (include *standard-cl-22*)\n;; Determine if number is odd\n  (defun-inline odd (X) (logand X 1))\n                ;; Actual collatz function\n  ;; determines number of step til 1\n  (defun collatz (N X zook)\n    (if (= X 1) ; We got 1\n      N ; Return the number of steps\n      (let ((incN (+ N 1))) ; Next N\n        (if (odd X) ; Is it odd?\n          (collatz incN (+ 1 (* 3 X))) ; Odd? 3 X + 1\n          (collatz incN (/ X 2)) ; Even? X / 2\n          )\n        )\n      )\n    )\n  (collatz 0 A) ; Run it\n  )".to_string();
    let changes = vec![TextDocumentContentChangeEvent {
        range_length: None,
        range: Some(Range {
            start: Position {
                character: 22,
                line: 13,
            },
            end: Position {
                character: 23,
                line: 13,
            },
        }),
        text: "".to_string(),
    }];
    let doc = (DocData {
        text: split_text(&content),
        version: -1,
    })
    .apply_patch(0, &changes);
    eprintln!("edited: {}", stringify_doc(&doc.text).unwrap());
    assert_eq!(stringify_doc(&doc.text).unwrap(), "(mod (A) ;;; COLLATZ conjecture\n\n;; set language standard\n  (include *standard-cl-22*)\n;; Determine if number is odd\n  (defun-inline odd (X) (logand X 1))\n                ;; Actual collatz function\n  ;; determines number of step til 1\n  (defun collatz (N X zook)\n    (if (= X 1) ; We got 1\n      N ; Return the number of steps\n      (let ((incN (+ N 1))) ; Next N\n        (if (odd X) ; Is it odd?\n          (collatz inc (+ 1 (* 3 X))) ; Odd? 3 X + 1\n          (collatz incN (/ X 2)) ; Even? X / 2\n          )\n        )\n      )\n    )\n  (collatz 0 A) ; Run it\n  )\n");
}

#[test]
fn test_patch_document_2() {
    let content = "(mod (A) ;;; COLLATZ conjecture\n\n;; set language standard\n  (include *standard-cl-22*)\n;; Determine if number is odd\n  (defun-inline odd (X) (logand X 1))\n                ;; Actual collatz function\n  ;; determines number of step til 1\n  (defun collatz (N X zook)\n    (if (= X 1) ; We got 1\n      N ; Return the number of steps\n      (let ((incN (+ N 1))) ; Next N\n        (if (odd X) ; Is it odd?\n          (collatz  (+ 1 (* 3 X))) ; Odd? 3 X + 1\n          (collatz incN (/ X 2)) ; Even? X / 2\n          )\n        )\n      )\n    )\n  (collatz 0 A) ; Run it\n  )".to_string();
    let changes = vec![TextDocumentContentChangeEvent {
        range_length: None,
        range: Some(Range {
            start: Position {
                character: 19,
                line: 13,
            },
            end: Position {
                character: 19,
                line: 13,
            },
        }),
        text: "z".to_string(),
    }];
    let doc = (DocData {
        text: split_text(&content),
        version: -1,
    })
    .apply_patch(1, &changes);
    eprintln!("edited: {}", stringify_doc(&doc.text).unwrap());
    assert_eq!(stringify_doc(&doc.text).unwrap(), "(mod (A) ;;; COLLATZ conjecture\n\n;; set language standard\n  (include *standard-cl-22*)\n;; Determine if number is odd\n  (defun-inline odd (X) (logand X 1))\n                ;; Actual collatz function\n  ;; determines number of step til 1\n  (defun collatz (N X zook)\n    (if (= X 1) ; We got 1\n      N ; Return the number of steps\n      (let ((incN (+ N 1))) ; Next N\n        (if (odd X) ; Is it odd?\n          (collatz z (+ 1 (* 3 X))) ; Odd? 3 X + 1\n          (collatz incN (/ X 2)) ; Even? X / 2\n          )\n        )\n      )\n    )\n  (collatz 0 A) ; Run it\n  )\n");
}

#[test]
fn test_patch_document_3() {
    let content = "(test\n  1\n  2\n  3)".to_string();
    let changes = vec![TextDocumentContentChangeEvent {
        range_length: None,
        range: Some(Range {
            start: Position {
                character: 0,
                line: 1,
            },
            end: Position {
                character: 0,
                line: 2,
            },
        }),
        text: "  *\n".to_string(),
    }];
    let doc = (DocData {
        text: split_text(&content),
        version: -1,
    })
    .apply_patch(1, &changes);
    eprintln!("edited: {}", stringify_doc(&doc.text).unwrap());
    assert_eq!(stringify_doc(&doc.text).unwrap(), "(test\n  *\n  2\n  3)\n");
}

#[test]
fn test_simple_ranges() {
    let content = "(mod ()\n  (defun F (X)\n    ()\n    )\n  (F 3)\n  )".to_string();
    let simple_ranges = make_simple_ranges(&split_text(&content));
    assert_eq!(
        simple_ranges,
        vec![
            DocRange {
                start: DocPosition {
                    line: 0,
                    character: 5
                },
                end: DocPosition {
                    line: 0,
                    character: 7
                }
            },
            DocRange {
                start: DocPosition {
                    line: 1,
                    character: 2,
                },
                end: DocPosition {
                    line: 3,
                    character: 5
                }
            },
            DocRange {
                start: DocPosition {
                    line: 4,
                    character: 2
                },
                end: DocPosition {
                    line: 4,
                    character: 7
                }
            }
        ]
    );
}

// Remove renamed scope info so we can compare.
fn chop_scopes(s: &str) -> String {
    let re = Regex::new("_\\$_[0-9]+").unwrap();
    re.replace_all(&s, "").to_string()
}

fn run_reparse_steps(
    loc: Srcloc,
    opts: Rc<dyn CompilerOpts>,
    file: &String,
    text_inputs: &[String],
) -> ParsedDoc {
    let mut doc = ParsedDoc::new(loc.clone());

    for content in text_inputs.iter() {
        let text = split_text(&content);
        let ranges = make_simple_ranges(&text);
        let reparsed = reparse_subset(
            opts.clone(),
            &text,
            &file,
            &ranges,
            &doc.compiled,
            &doc.hashes,
        );
        doc = combine_new_with_old_parse(&file, &text, &doc, &reparsed);
    }

    doc
}

#[test]
fn test_reparse_subset_1() {
    let file = "file:///test.cl".to_string();
    let loc = Srcloc::start(&file);
    let opts = Rc::new(DefaultCompilerOpts::new(&file));
    let combined = run_reparse_steps(
        loc,
        opts.clone(),
        &file,
        &["(mod X (defun F (X) (+ X 1)) (F X))".to_string()],
    );
    assert_eq!(
        "(X (defun F (X) (+ X 1)) (F X))",
        chop_scopes(&combined.compiled.to_sexp().to_string())
    );
}

#[test]
fn test_reparse_subset_2() {
    let file = "file:///test.cl".to_string();
    let loc = Srcloc::start(&file);
    let opts = Rc::new(DefaultCompilerOpts::new(&file));
    let combined = run_reparse_steps(
        loc,
        opts.clone(),
        &file,
        &["(mod X (defun F (X) (+ X 1)) X)".to_string()],
    );
    assert_eq!(
        "(X (defun F (X) (+ X 1)) X)",
        chop_scopes(&combined.compiled.to_sexp().to_string())
    );
}

#[test]
fn test_reparse_subset_3() {
    let file = "file:///test.cl".to_string();
    let loc = Srcloc::start(&file);
    let opts = Rc::new(DefaultCompilerOpts::new(&file));
    let combined2 = run_reparse_steps(
        loc,
        opts.clone(),
        &file,
        &[
            "(mod X (defun F (X) (+ X 1)) X)".to_string(),
            "(mod X (defun G (X) (+ X 1)) X)".to_string(),
        ],
    );
    assert_eq!(
        "(X (defun G (X) (+ X 1)) X)",
        chop_scopes(&combined2.compiled.to_sexp().to_string())
    );
}

#[test]
fn test_reparse_subset_4() {
    let file = "file:///test.cl".to_string();
    let loc = Srcloc::start(&file);
    let opts = Rc::new(DefaultCompilerOpts::new(&file));
    let combined = run_reparse_steps(
        loc,
        opts.clone(),
        &file,
        &["(mod X (include test.clib) (defun F (X) (+ X 1)) X)".to_string()],
    );
    assert_eq!(
        "(X (defun F (X) (+ X 1)) X)",
        chop_scopes(&combined.compiled.to_sexp().to_string())
    );
}

// Warn on unrecognized function call.
#[test]
fn test_warn_call_of_undefined_function() {
    let file = "file:///test.cl".to_string();
    let loc = Srcloc::start(&file);
    let opts = Rc::new(DefaultCompilerOpts::new(&file));
    let combined = run_reparse_steps(loc, opts.clone(), &file, &["(mod X (F 3))".to_string()]);
    assert_eq!(!combined.errors.is_empty(), true);
}

#[test]
fn test_mod_ends_in_defun_error() {
    let file = "file:///test.cl".to_string();
    let loc = Srcloc::start(&file);
    let opts = Rc::new(DefaultCompilerOpts::new(&file));
    let combined = run_reparse_steps(
        loc,
        opts.clone(),
        &file,
        &[
            "(mod X (defun F (X) (+ X 1)))".to_string(),
        ],
    );
    assert_eq!(combined.errors.len(), 1);
}

#[test]
fn test_list_ends_in_defun_no_error() {
    let file = "file:///test.cl".to_string();
    let loc = Srcloc::start(&file);
    let opts = Rc::new(DefaultCompilerOpts::new(&file));
    let combined = run_reparse_steps(
        loc,
        opts.clone(),
        &file,
        &[
            "( (defun F (X) (+ X 1)) )".to_string(),
        ],
    );
    assert_eq!(combined.errors.is_empty(), true);
}

#[test]
fn test_mod_can_cease_reporting_wrong_function_error() {
    let file = "file:///test.cl".to_string();
    let loc = Srcloc::start(&file);
    let opts = Rc::new(DefaultCompilerOpts::new(&file));
    let combined = run_reparse_steps(
        loc,
        opts.clone(),
        &file,
        &[
            "(mod X (defun F (X) (+ X 1)))".to_string(),
            "(mod X (defun F (X) (+ X 1)) (G X))".to_string(),
            "(mod X (defun G (X) (+ X 1)) (G X))".to_string(),
        ],
    );
    assert_eq!(combined.errors.len(), 0);
}
