use std::error::Error;

use lsp_server::{Connection, Message};

use clvm_tools_rs::compiler::lsp::{
    LSPServiceProvider,
    LSPServiceMessageHandler
};

fn main_loop(
    connection: Connection
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let mut lsp_provider = LSPServiceProvider::new(false);

    eprintln!("starting example main loop");
    for msg in &connection.receiver {
        if let Message::Request(req) = &msg {
            if connection.handle_shutdown(req)? {
                return Ok(());
            }
        }

        let out_msgs = lsp_provider.handle_message(&msg).map_err(Box::<dyn Error + Send + Sync>::from)?;
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
