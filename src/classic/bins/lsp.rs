use std::cmp::Ordering;
use std::error::Error;

use lsp_types::OneOf;

use lsp_server::{Connection, ExtractError, Message, Request, RequestId, Response};

use clvm_tools_rs::compiler::compiler::DefaultCompilerOpts;
use clvm_tools_rs::compiler::comptypes::{
    BodyForm, CompileErr, CompilerOpts, CompileForm, HelperForm, LetFormKind
};
use clvm_tools_rs::compiler::frontend::frontend;
use clvm_tools_rs::compiler::lsp::{
    TOKEN_TYPES,
    TOKEN_MODIFIERS,
    LSPServiceProvider,
    LSPServiceMessageHandler
};
use clvm_tools_rs::compiler::sexp::{SExp, parse_sexp};
use clvm_tools_rs::compiler::srcloc::Srcloc;

fn main_loop(
    connection: Connection,
    params: serde_json::Value,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let mut lsp_provider = LSPServiceProvider::new();

    eprintln!("starting example main loop");
    for msg in &connection.receiver {
        if let Message::Request(req) = &msg {
            if connection.handle_shutdown(&req)? {
                return Ok(());
            }
        }

        let out_msgs = lsp_provider.handle_message(&msg).map_err(|e| Box::<dyn Error + Send + Sync>::from(e))?;
        for o in out_msgs.into_iter() {
            connection.sender.send(o)?;
        }
    }
    Ok(())
}

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
    let mut completion_start = Vec::new();
    for i in 65..65+27 {
        completion_start.push(i as u8);
    }
    for i in 97..97+27 {
        completion_start.push(i as u8);
    }
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
        completion_provider: Some(CompletionOptions {
            resolve_provider: Some(true),
//             trigger_characters: Some(completion_start),
            ..Default::default()
        }),
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
