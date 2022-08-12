use std::collections::HashMap;
use std::error::Error;
use std::rc::Rc;

use lazy_static::lazy_static;
use serde_json::value::{Value, to_value};

use lsp_server::ResponseError;
use lsp_types::OneOf;
use lsp_types::{
    request::GotoDefinition,
    request::SemanticTokensFullRequest,
    notification::DidOpenTextDocument,
    ClientCapabilities,
    DidOpenTextDocumentParams,
    GotoDefinitionResponse,
    InitializeParams,
    SemanticToken,
    SemanticTokenModifier,
    SemanticTokens,
    SemanticTokensFullOptions,
    SemanticTokensLegend,
    SemanticTokensOptions,
    SemanticTokensServerCapabilities,
    SemanticTokenType,
    ServerCapabilities,
    TextDocumentSyncCapability,
    TextDocumentSyncKind,
    WorkDoneProgressOptions,
};

use lsp_server::{Connection, ExtractError, Message, Request, RequestId, Response};

use clvm_tools_rs::compiler::compiler::DefaultCompilerOpts;
use clvm_tools_rs::compiler::comptypes::{
    CompileErr, CompilerOpts, CompileForm, HelperForm
};
use clvm_tools_rs::compiler::frontend::frontend;
use clvm_tools_rs::compiler::sexp::parse_sexp;
use clvm_tools_rs::compiler::srcloc::Srcloc;

lazy_static! {
    pub static ref TOKEN_TYPES: Vec<SemanticTokenType> = {
        vec![
            SemanticTokenType::PARAMETER,
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

const TK_PARAMETER_IDX: u32 = 0;
const TK_FUNCTION_IDX: u32 = 1;
const TK_MACRO_IDX: u32 = 2;
const TK_KEYWORD_IDX: u32 = 3;
const TK_COMMENT_IDX: u32 = 4;
const TK_STRING_IDX: u32 = 5;
const TK_NUMBER_IDX: u32 = 6;
const TK_OPERATOR_IDX: u32 = 7;

const TK_DEFINITION_BIT: u32 = 0;
const TK_READONLY_BIT: u32 = 1;
const TK_DOCUMENTATION_BIT: u32 = 2;

fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
    // Note that  we must have our logging only write out to stderr.
    eprintln!("starting chialisp LSP server");

    // Create the transport. Includes the stdio (stdin and stdout) versions but this could
    // also be implemented to use sockets or HTTP.
    let (connection, io_threads) = Connection::stdio();

    // Run the server
    eprintln!("start initialization");
    let (id, params) = connection.initialize_start()?;

    let init_params: InitializeParams = serde_json::from_value(params).unwrap();
    let client_capabilities: ClientCapabilities = init_params.capabilities;
    let server_capabilities = ServerCapabilities {
        // Specify capabilities from the set:
        // https://docs.rs/lsp-types/latest/lsp_types/struct.ServerCapabilities.html
        definition_provider: Some(OneOf::Left(true)),
        semantic_tokens_provider: Some(SemanticTokensServerCapabilities::SemanticTokensOptions(SemanticTokensOptions {
            work_done_progress_options: WorkDoneProgressOptions {
                work_done_progress: Some(false)
            },
            legend: SemanticTokensLegend {
                token_types: TOKEN_TYPES.clone(),
                token_modifiers: TOKEN_MODIFIERS.clone(),
            },
            range: None,
            full: Some(SemanticTokensFullOptions::Delta {delta: Some(true)})
        })),
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        ..Default::default()
    };

    let initialize_data = serde_json::json!({
        "capabilities": server_capabilities,
        "serverInfo": {
            "name": "lsp-server-test",
            "version": "0.1"
        }
    });

    eprintln!("end initialization sending? {}", initialize_data.to_string());
    connection.initialize_finish(id, initialize_data.clone())?;

    eprintln!("done init");
    main_loop(connection, initialize_data)?;
    io_threads.join()?;

    // Shut down gracefully.
    eprintln!("shutting down server");
    Ok(())
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

    fn new(opts: Rc<CompilerOpts>, file: &String, srctext: &String) -> Self {
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

fn do_semantic_tokens(id: RequestId, frontend: &CompileForm) -> Response {
    let mut last_row = 1;
    let mut last_col = 1;
    let mut result = SemanticTokens {
        result_id: None,
        data: Vec::new(),
    };
    for form in frontend.helpers.iter() {
        match form {
            HelperForm::Defun(_,defun) => {
                eprintln!("handling form {}", form.to_sexp().to_string());
                if defun.nl.line != last_row {
                    last_col = 1;
                    let st = SemanticToken {
                        delta_line: (defun.nl.line - last_row) as u32,
                        delta_start: (defun.nl.col - last_col) as u32,
                        length: defun.nl.len() as u32,
                        token_type: TK_FUNCTION_IDX,
                        token_modifiers_bitset: TK_DEFINITION_BIT
                    };
                    last_row = defun.nl.line;
                    last_col = defun.nl.col + defun.nl.len();
                    result.data.push(st);
                }
            },
            _ => { }
        }
    }

    Response {
        id,
        error: None,
        result: Some(serde_json::to_value(result).unwrap())
    }
}

fn main_loop(
    connection: Connection,
    params: serde_json::Value,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let params: InitializeParams = serde_json::from_value(params).unwrap();
    let mut document_collection: HashMap<String, DocData> = HashMap::new();
    let mut parsed_documents: HashMap<String, ParsedDoc> = HashMap::new();

    eprintln!("starting example main loop");
    for msg in &connection.receiver {
        eprintln!("got msg: {:?}", msg);
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                if let Ok((id, params)) = cast::<SemanticTokensFullRequest>(req.clone()) {
                    eprintln!("got semantic token request #{}: for file {}", id, params.text_document.uri.to_string());
                    let uristring = params.text_document.uri.to_string();
                    if parsed_documents.get(&uristring).is_none() {
                        eprintln!("ensure parsed");
                        ensure_parsed_document(
                            &document_collection,
                            &mut parsed_documents,
                            &uristring
                        );
                    }
                    if let Some(parsed) = parsed_documents.get(&uristring) {
                        eprintln!("parsed {:?}", parsed);
                        match &parsed.result {
                            ParseResult::Completed(frontend) => {
                                let resp = do_semantic_tokens(id, &frontend);
                                connection.sender.send(Message::Response(resp))?;
                            },
                            ParseResult::WithError(error) => {
                                let resp = Response { id, result: None, error: Some(ResponseError {
                                    code: 1,
                                    data: None,
                                    message: format!("{}: {}", error.0.to_string(), error.1)
                                }) };
                                connection.sender.send(Message::Response(resp))?;
                            }
                        }
                    } else {
                        eprintln!("no compile output :-(");
                    }
                } else if let Ok((id, params)) = cast::<GotoDefinition>(req.clone()) {
                    eprintln!("got gotoDefinition request #{}: {:?}", id, params);
                    let result = Some(GotoDefinitionResponse::Array(Vec::new()));
                    let result = serde_json::to_value(&result).unwrap();
                    let resp = Response { id, result: Some(result), error: None };
                    connection.sender.send(Message::Response(resp))?;
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
                        document_collection.insert(params.text_document.uri.to_string(), DocData { text: params.text_document.text.clone() });
                    } else {
                        eprintln!("cast failed in didOpen");
                    }
                } else {
                    eprintln!("not sure what we got: {:?}", not);
                }
            }
            Message::Notification(not) => {
                eprintln!("got notification: {:?}", not);
            }
        }
    }
    Ok(())
}

fn cast<R>(req: Request) -> Result<(RequestId, R::Params), ExtractError<Request>>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}
