use std::fs;
use std::io;
use std::process;
use subprocess::{Exec, Redirection};
use stdio_override::StdoutOverride;
use shlex::split;
use tempfile::tempdir;

use clvmr::allocator::Allocator;

use clvm_tools_rs::classic::clvm::__type_compatibility__::Stream;
use clvm_tools_rs::classic::clvm_tools::cmds::{call_tool, launch_tool};
use clvm_tools_rs::compiler::sexp::decode_string;

fn main() {
    let allowed_tools: Vec<String> = ["brun","opc","opd"].iter().map(|x| x.to_string()).collect();
    for input_line_osstr in io::stdin().lines() {
        let input_line = input_line_osstr.unwrap();
        let exec_result = Exec::shell(&input_line).stdout(Redirection::Pipe).stderr(Redirection::Pipe).capture();
        let result = match exec_result {
            Ok(r) => r,
            Err(_) => { continue; }
        };

        let mut python_got_back = result.stdout_str();
        if python_got_back.is_empty() {
            python_got_back = result.stderr_str();
        }
        let args = match split(&input_line) {
            Some(a) => a,
            _ => { continue; }
        };

        if args.is_empty() || !allowed_tools.contains(&args[0]) {
            continue;
        }

        let mut stdout_result;
        {
            let redir_path = tempdir().unwrap();
            let redir_name = redir_path.path().join("tmp-out.txt");
            {
                let _guard = StdoutOverride::override_file(redir_name.clone()).unwrap();
                if args[0] == "opc" || args[0] == "opd" {
                    let mut stream = Stream::new(None);
                    let mut allocator = Allocator::new();
                    let call_result = call_tool(&mut stream, &mut allocator, &args[0], &args);
                    match call_result {
                        Ok(_) => {
                            stdout_result = decode_string(stream.get_value().data());
                        },
                        Err(e) => {
                            stdout_result = e.to_string();
                        }
                    }
                } else if args[0] == "brun" {
                    let mut stream = Stream::new(None);
                    launch_tool(&mut stream, &args, &args[0], 0);
                    stdout_result = decode_string(stream.get_value().data());
                } else {
                    let mut stream = Stream::new(None);
                    launch_tool(&mut stream, &args, &args[0], 2);
                    stdout_result = decode_string(stream.get_value().data());
                }
                if stdout_result.is_empty() {
                    stdout_result = fs::read_to_string(redir_name).unwrap();
                }
            }
        }

        if stdout_result.trim() != python_got_back.trim() {
            eprintln!("mismatched output\n rust  {}\nvs\npython {}", stdout_result, python_got_back);
            process::abort();
        }
    }
}
