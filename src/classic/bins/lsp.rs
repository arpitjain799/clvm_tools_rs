use std::collections::HashMap;
use std::error::Error;
use std::rc::Rc;

use lsp_types::OneOf;
use lsp_types::{
    request::GotoDefinition,
    request::SemanticTokensFullRequest,
    ClientCapabilities,
    GotoDefinitionResponse,
    InitializeParams,
    SemanticTokenModifier,
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
                token_types: vec![
                    SemanticTokenType::PARAMETER,
                    SemanticTokenType::FUNCTION,
                    SemanticTokenType::MACRO,
                    SemanticTokenType::KEYWORD,
                    SemanticTokenType::COMMENT,
                    SemanticTokenType::STRING,
                    SemanticTokenType::NUMBER,
                    SemanticTokenType::OPERATOR
                ],
                token_modifiers: vec![
                    SemanticTokenModifier::DEFINITION,
                    SemanticTokenModifier::READONLY,
                    SemanticTokenModifier::DOCUMENTATION
                ],
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

struct DocData { }

fn main_loop(
    connection: Connection,
    params: serde_json::Value,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let params: InitializeParams = serde_json::from_value(params).unwrap();
    let mut documentCollection: HashMap<String, DocData> = HashMap::new();

    eprintln!("starting example main loop");
    for msg in &connection.receiver {
        eprintln!("got msg: {:?}", msg);
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                if let Ok((id, params)) = cast::<SemanticTokensFullRequest>(req.clone()) {
                    eprintln!("got semantic token request #{}: {:?}", id, params);
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
