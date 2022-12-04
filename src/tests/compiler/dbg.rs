use std::collections::HashMap;
use std::rc::Rc;

use crate::classic::clvm_tools::stages::stage_0::DefaultProgramRunner;
use crate::compiler::dbg::handler::Debugger;
use crate::compiler::dbg::server::MessageBuffer;
use crate::compiler::dbg::types::{EPrintWriter, FSFileReader};
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
    let debugger = Debugger::new(fs, log, runner.clone(), prims.clone());
    let mut service = MessageBuffer::new(debugger);
    let init_msg = "{\"command\":\"initialize\",\"arguments\":{\"clientID\":\"vscode\",\"clientName\":\"Visual Studio Code\",\"adapterID\":\"chialisp-dbg\",\"pathFormat\":\"path\",\"linesStartAt1\":true,\"columnsStartAt1\":true,\"supportsVariableType\":true,\"supportsVariablePaging\":true,\"supportsRunInTerminalRequest\":true,\"locale\":\"en-us\",\"supportsProgressReporting\":true,\"supportsInvalidatedEvent\":true,\"supportsMemoryReferences\":true,\"supportsArgsCanBeInterpretedByShell\":true},\"type\":\"request\",\"seq\":1}".to_string();
    let outmsgs = service.process_message(&init_msg.as_bytes()).unwrap();
    assert_eq!(outmsgs.unwrap().len(), 1);
}

#[test]
fn dap_can_launch() {
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
    let debugger = Debugger::new(fs, log, runner.clone(), prims.clone());
    let mut service = MessageBuffer::new(debugger);
    let init_msg = "{\"command\":\"initialize\",\"arguments\":{\"clientID\":\"vscode\",\"clientName\":\"Visual Studio Code\",\"adapterID\":\"chialisp-dbg\",\"pathFormat\":\"path\",\"linesStartAt1\":true,\"columnsStartAt1\":true,\"supportsVariableType\":true,\"supportsVariablePaging\":true,\"supportsRunInTerminalRequest\":true,\"locale\":\"en-us\",\"supportsProgressReporting\":true,\"supportsInvalidatedEvent\":true,\"supportsMemoryReferences\":true,\"supportsArgsCanBeInterpretedByShell\":true},\"type\":\"request\",\"seq\":1}".to_string();
    let outmsgs = service.process_message(&init_msg.as_bytes()).unwrap();
    assert_eq!(outmsgs.unwrap().len(), 1);
    let launch_msg = "{\"command\":\"launch\",\"arguments\":{\"name\":\"testempty.cl\",\"type\":\"chialisp-dbg\",\"request\":\"launch\",\"stopOnEntry\":true,\"yieldSteps\":4096,\"program\":\"resources/tests/testdbg.cl\",\"__sessionId\":\"2c6729c4-1067-4cce-8ca3-e5f4275fe1d7\"},\"type\":\"request\",\"seq\":2}".to_string();
    let outmsgs2 = service.process_message(&launch_msg.as_bytes()).unwrap();
    assert_eq!(outmsgs2.unwrap().len(), 2);
}
