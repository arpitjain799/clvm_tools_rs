#![no_main]

use rand::prelude::*;

use std::fs;
use std::process::Command;
use std::rc::Rc;

use libfuzzer_sys::fuzz_target;
use clvm_tools_rs::compiler::compiler::DefaultCompilerOpts;
use clvm_tools_rs::compiler::frontend::frontend;
use clvm_tools_rs::compiler::fuzzer::FuzzProgram;
use clvm_tools_rs::compiler::sexp::decode_string;
use clvm_tools_rs::compiler::typechia::{standard_type_context};
use clvm_tools_rs::fuzzing::fuzzrng::FuzzPseudoRng;
use clvm_tools_rs::fuzzing::purescript::chialisp_to_purescript;

fuzz_target!(|data: &[u8]| {
    let mut rng = FuzzPseudoRng::new(data);
    let prog: FuzzProgram = rng.gen();
    let serialized = prog.to_sexp();
    eprintln!("-- program {}", serialized);
    let opts = Rc::new(DefaultCompilerOpts::new("*random*"));
    let parsed = frontend(opts.clone(), vec![Rc::new(serialized)]).unwrap();

    let program = chialisp_to_purescript(opts.clone(), &parsed).expect("should generate");
    eprintln!("program {}", program);

    // Write program
    fs::write("type-test/src/Main.purs", program).expect("should write file");

    // Run test
    let output =
        Command::new("spago").
        current_dir("type-test").
        arg("build").
        output().
        expect("should build");

    let context = standard_type_context();
    let check_result = context.typecheck_chialisp_program(&parsed);

    if check_result.is_err() {
        if output.status.success() {
            eprintln!("stdout ---\n{}", decode_string(&output.stdout));
            eprintln!("stderr ---\n{}", decode_string(&output.stderr));
            panic!("generating well poorly typed program worked");
        }
    } else {
        if !output.status.success() {
            eprintln!("stdout ---\n{}", decode_string(&output.stdout));
            eprintln!("stderr ---\n{}", decode_string(&output.stderr));
            panic!("generating well typed program failed");
        }
    }
});
