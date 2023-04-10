use std::io;
use std::process;
use subprocess::Exec;
use shlex::split;

use clvmr::allocator::Allocator;

use clvm_tools_rs::classic::clvm::__type_compatibility__::Stream;
use clvm_tools_rs::classic::clvm_tools::cmds::{call_tool, launch_tool};
use clvm_tools_rs::compiler::sexp::decode_string;

fn main() {
    let mut input_line = "".to_string();
    while let Ok(_) = io::stdin().read_line(&mut input_line) {
        let exec_result = Exec::shell(&input_line).capture();
        if let Ok(result) = exec_result {
            let python_got_back = result.stdout_str();
            let args = split(&input_line).unwrap();
            eprintln!("args {args:?}");
            if args.is_empty() {
                continue;
            }
            let stdout_result;
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

            if stdout_result != python_got_back {
                eprintln!("mismatched output\n rust  {}\nvs\npython {}", stdout_result, python_got_back);
                process::abort();
            }
        } else {
            eprintln!("python failed");
        }
    }
}
