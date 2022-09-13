use std::cmp::Ordering;
use std::error::Error;

use lsp_types::{
    InitializeParams,
    ClientCapabilities
};

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
    connection: Connection
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let mut lsp_provider = LSPServiceProvider::new(false);

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
    main_loop(connection)?;
    io_threads.join()?;

    // Shut down gracefully.
    eprintln!("shutting down server");
    Ok(())
}
