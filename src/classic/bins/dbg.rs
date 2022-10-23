use std::collections::HashMap;
use std::error::Error;
use std::io;
use std::io::{Read, Write};
use std::rc::Rc;

use clvm_tools_rs::classic::clvm_tools::stages::stage_0::DefaultProgramRunner;
use clvm_tools_rs::compiler::dbg::handler::Debugger;
use clvm_tools_rs::compiler::dbg::server::MessageBuffer;
use clvm_tools_rs::compiler::lsp::types::{EPrintWriter, FSFileReader};
use clvm_tools_rs::compiler::prims::prims;

fn main_loop(connection: &mut MessageBuffer<Debugger>) -> Result<(), Box<dyn Error + Sync + Send>> {
    eprintln!("starting example main loop");
    let mut read_buf: [u8; 4096] = [0; 4096];

    loop {
        if connection.is_eof() {
            return Ok(());
        }

        let res = io::stdin().read(&mut read_buf)?;

        let out_bytes = connection.receive_bytes(&read_buf[0..res])?;
        if let Some(out) = out_bytes {
            io::stdout().write_all(&out)?;
        }
        //.map_err(Box::<dyn Error + Send + Sync>::from)?
    }
}

fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
    // Note that  we must have our logging only write out to stderr.
    eprintln!("starting chialisp LSP server");

    // Create the transport. Includes the stdio (stdin and stdout) versions but this could
    // also be implemented to use sockets or HTTP.
    let simple_prims = prims();
    let mut prim_map = HashMap::new();

    for (name, sexp) in simple_prims.iter() {
        prim_map.insert(name.clone(), Rc::new(sexp.clone()));
    }

    let dbg_provider = Debugger::new(
        Rc::new(FSFileReader::new()),
        Rc::new(EPrintWriter::new()),
        Rc::new(DefaultProgramRunner::new()),
        Rc::new(prim_map)
    );
    let mut connection = MessageBuffer::new(dbg_provider);

    // Run the server
    main_loop(&mut connection)?;

    // Shut down gracefully.
    eprintln!("shutting down server");
    Ok(())
}
