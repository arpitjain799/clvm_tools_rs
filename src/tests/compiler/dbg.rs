use std::collections::HashMap;
use std::rc::Rc;

use crate::classic::clvm_tools::stages::stage_0::DefaultProgramRunner;
use crate::compiler::dbg::handler::Debugger;
use crate::compiler::dbg::server::MessageBuffer;
use crate::compiler::lsp::types::{EPrintWriter, FSFileReader};
use crate::compiler::prims;

#[test]
fn dap_can_initialize() {
    let fs = Rc::new(FSFileReader::new());
    let log = Rc::new(EPrintWriter::new());

    // Get prims
    let simple_prims = prims::prims();
    let mut prim_map = HashMap::new();

    for (name, sexp) in simple_prims.iter() {
        prim_map.insert(name.clone(), Rc::new(sexp.clone()));
    }

    let prims = Rc::new(prim_map);
    let runner = Rc::new(DefaultProgramRunner::new());
    let debugger = Debugger::new(
        fs,
        log,
        runner.clone(),
        prims.clone(),
    );
    let mut service = MessageBuffer::new(debugger);
    let init_msg = "{\"command\":\"initialize\",\"arguments\":{\"clientID\":\"vscode\",\"clientName\":\"Visual Studio Code\",\"adapterID\":\"chialisp-dbg\",\"pathFormat\":\"path\",\"linesStartAt1\":true,\"columnsStartAt1\":true,\"supportsVariableType\":true,\"supportsVariablePaging\":true,\"supportsRunInTerminalRequest\":true,\"locale\":\"en-us\",\"supportsProgressReporting\":true,\"supportsInvalidatedEvent\":true,\"supportsMemoryReferences\":true,\"supportsArgsCanBeInterpretedByShell\":true},\"type\":\"request\",\"seq\":1}".to_string();
    let outmsgs = service.process_message(&init_msg.as_bytes()).unwrap();
    assert_eq!(outmsgs.unwrap().len(), 1);
}
