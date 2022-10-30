use serde::Deserialize;

use std::collections::HashMap;
use std::io::BufRead;
use std::rc::Rc;

use debug_types::events::{Event, EventBody, StoppedEvent, StoppedReason};
use debug_types::requests::{InitializeRequestArguments, LaunchRequestArguments, RequestCommand};
use debug_types::responses::{InitializeResponse, Response, ResponseBody, ThreadsResponse};
use debug_types::types::{Capabilities, ChecksumAlgorithm, Thread};
use debug_types::{MessageKind, ProtocolMessage};

use crate::classic::clvm_tools::stages::stage_0::TRunProgram;
use crate::compiler::cldb::{CldbNoOverride, CldbRun, CldbRunEnv};
use crate::compiler::clvm::start_step;
use crate::compiler::dbg::types::MessageHandler;
use crate::compiler::lsp::types::{IFileReader, ILogWriter};
use crate::compiler::sexp::{SExp, parse_sexp};
use crate::compiler::srcloc::Srcloc;

#[derive(Clone, Debug, Deserialize)]
pub struct ExtraLaunchData {
    #[serde(rename = "stopOnEntry")]
    stop_on_entry: bool
}

#[derive(Clone, Debug, Deserialize)]
pub struct RequestContainer<T> {
    arguments: T
}

pub struct RunningDebugger {
    initialized: InitializeRequestArguments,
    launch_info: LaunchRequestArguments,
    running: bool,
    run: CldbRun
}

pub enum State {
    PreInitialization,
    Initialized(InitializeRequestArguments),
    Launched(RunningDebugger)
}

pub enum BreakpointLocation {
    Srcloc(Srcloc),
    Treehash(String),
}

pub struct Debugger {
    // External interface
    pub fs: Rc<dyn IFileReader>,
    pub log: Rc<dyn ILogWriter>,

    pub state: State,
    // We'll store a short program here for how to run the target program.
    pub expression: Option<Rc<SExp>>,
    pub variable_values: HashMap<Vec<u8>, Rc<SExp>>,
    // All historical steps (for backstep)
    pub steps: Vec<HashMap<String, String>>,
    pub breakpoints: Vec<BreakpointLocation>,

    pub runner: Rc<dyn TRunProgram>,
    pub prim_map: Rc<HashMap<Vec<u8>, Rc<SExp>>>,
    pub msg_seq: i64,
}

impl Debugger {
    pub fn new(
        fs: Rc<dyn IFileReader>,
        log: Rc<dyn ILogWriter>,
        runner: Rc<dyn TRunProgram>,
        prim_map: Rc<HashMap<Vec<u8>, Rc<SExp>>>,
    ) -> Self {
        Debugger {
            fs,
            log,
            state: State::PreInitialization,
            expression: None,
            variable_values: HashMap::new(),
            steps: Vec::new(),
            breakpoints: Vec::new(),
            runner,
            prim_map,
            msg_seq: 0,
        }
    }
}

fn get_initialize_response() -> InitializeResponse {
    InitializeResponse {
        capabilities: Capabilities {
            supports_configuration_done_request: Some(true),
            supports_function_breakpoints: Some(true),
            supports_conditional_breakpoints: None,
            supports_hit_conditional_breakpoints: None,
            supports_evaluate_for_hovers: None,
            exception_breakpoint_filters: None,
            supports_step_back: Some(true),
            supports_goto_targets_request: None,
            supports_step_in_targets_request: None,
            supports_completions_request: None,
            supports_modules_request: None,
            completion_trigger_characters: None,
            additional_module_columns: None,
            supported_checksum_algorithms: Some(vec![ChecksumAlgorithm::SHA256]),
            support_suspend_debuggee: None,
            support_terminate_debuggee: Some(true),
            supports_breakpoint_locations_request: Some(true),
            supports_cancel_request: None,
            supports_clipboard_context: None,
            supports_data_breakpoints: Some(true),
            supports_delayed_stack_trace_loading: None,
            supports_disassemble_request: Some(true),
            supports_exception_filter_options: None,
            supports_exception_info_request: None,
            supports_exception_options: None,
            supports_instruction_breakpoints: Some(true),
            supports_loaded_sources_request: Some(true),
            supports_log_points: Some(true),
            supports_read_memory_request: None,
            supports_restart_frame: None,
            supports_restart_request: Some(true),
            supports_set_expression: None,
            supports_set_variable: None,
            supports_single_thread_execution_requests: None,
            supports_stepping_granularity: None,
            supports_terminate_request: Some(true),
            supports_terminate_threads_request: None,
            supports_value_formatting_options: Some(true),
            supports_write_memory_request: None,
        },
    }
}

impl MessageHandler<ProtocolMessage> for Debugger {
    fn handle_message(
        &mut self,
        raw_json: &serde_json::Value,
        pm: &ProtocolMessage,
    ) -> Result<Option<Vec<ProtocolMessage>>, String> {
        self.log.write(&format!("got message {}", serde_json::to_string(pm).unwrap()));
        if let MessageKind::Request(req) = &pm.message {
            match (&self.state, req) {
                (State::PreInitialization, RequestCommand::Initialize(irq)) => {
                    self.state = State::Initialized(irq.clone());
                    self.msg_seq += 1;
                    return Ok(Some(vec![ProtocolMessage {
                        seq: self.msg_seq,
                        message: MessageKind::Response(Response {
                            request_seq: pm.seq,
                            success: true,
                            message: None,
                            body: Some(ResponseBody::Initialize(get_initialize_response())),
                        }),
                    }]));
                }
                (State::Initialized(i), RequestCommand::Launch(l)) => {
                    let launch_extra: Option<RequestContainer<ExtraLaunchData>> =
                        serde_json::from_value(raw_json.clone()).
                        map(Some).unwrap_or(None);
                    let stop_on_entry =
                        launch_extra.map(|l| l.arguments.stop_on_entry).unwrap_or(false);

                    self.log.write(&format!("stop on entry: {}", stop_on_entry));

                    if let Some(name) = &l.name {
                        let read_in_file = self.fs.read(&name)?;
                        let parsed_program =
                            parse_sexp(
                                Srcloc::start(&name),
                                read_in_file.iter().copied()
                            ).map_err(|e| format!("{}: {}", e.0.to_string(), e.1))?;
                        if parsed_program.is_empty() {
                            return Err(format!("Empty program file {}", name));
                        }

                        // XXX Empty arguments for now.
                        let arguments = Rc::new(SExp::Nil(parsed_program[0].loc()));
                        let program_lines: Vec<String> =
                            read_in_file.lines().map(|x| x.unwrap().to_string()).collect();
                        let env = CldbRunEnv::new(
                            Some(name.to_owned()),
                            program_lines,
                            Box::new(CldbNoOverride::new())
                        );
                        let cldb_run = CldbRun::new(
                            self.runner.clone(),
                            self.prim_map.clone(),
                            Box::new(env),
                            start_step(parsed_program[0].clone(), arguments)
                        );
                        self.state = State::Launched(RunningDebugger {
                            initialized: i.clone(),
                            launch_info: l.clone(),
                            running: !stop_on_entry,
                            run: cldb_run
                        });
                        self.msg_seq += 1;

                        let mut out_messages = vec![ProtocolMessage {
                            seq: self.msg_seq,
                            message: MessageKind::Response(Response {
                                request_seq: pm.seq,
                                success: true,
                                message: None,
                                body: Some(ResponseBody::Launch)
                            })
                        }];

                        // Signal that we're paused if stop on entry.
                        if stop_on_entry {
                            self.msg_seq += 1;
                            out_messages.push(ProtocolMessage {
                                seq: self.msg_seq,
                                message: MessageKind::Event(Event {
                                    body: Some(EventBody::Stopped(StoppedEvent {
                                        reason: StoppedReason::Entry,
                                        description: None,
                                        thread_id: None,
                                        preserve_focus_hint: None,
                                        text: None,
                                        all_threads_stopped: Some(true),
                                        hit_breakpoint_ids: None
                                    }))
                                })
                            });
                        }

                        return Ok(Some(out_messages));
                    } else {
                        self.log.write("No program provided");
                    }
                },
                (State::Launched(r), RequestCommand::Threads) => {
                    return Ok(Some(vec![ProtocolMessage {
                        seq: self.msg_seq,
                        message: MessageKind::Response(Response {
                            request_seq: pm.seq,
                            success: true,
                            message: None,
                            body: Some(ResponseBody::Threads(ThreadsResponse {
                                threads: vec![Thread {
                                    id: 1,
                                    name: "main".to_string()
                                }]
                            }))
                        })
                    }]));
                },
                (State::Launched(r), RequestCommand::StackTrace(sta)) => {
                    
                },
                (State::Launched(r), RequestCommand::StepIn(si)) => {
                },
                (State::Launched(r), RequestCommand::Next(n)) => {
                },
                (State::Launched(r), RequestCommand::StepOut(so)) => {
                },
                (State::Launched(r), RequestCommand::Continue(c)) => {
                },
                (_st, _rq) => {
                    self.log.write(&format!("Don't know what to do with {:?}", req));
                }
            }
        }

        self.log.write(&format!("unhandled message {:?}", pm));
        return Err(format!("unhandled message {:?}", pm));
    }
}
