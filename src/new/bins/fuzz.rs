use std::collections::HashMap;
use std::rc::Rc;

use clvmr::allocator::Allocator;

use clvm_tools_rs::classic::clvm_tools::stages::stage_0::DefaultProgramRunner;

use clvm_tools_rs::compiler::prims;
use clvm_tools_rs::compiler::clvm::run;
use clvm_tools_rs::compiler::compiler::{ DefaultCompilerOpts, compile_file };

use clvm_tools_rs::compiler::fuzzer::FuzzProgram;
use clvm_tools_rs::compiler::runtypes::RunFailure;

use rand::random;

fn main() {
    let mut allocator = Allocator::new();
    let opts = Rc::new(DefaultCompilerOpts::new(&"*prog*".to_string()));
    let runner = Rc::new(DefaultProgramRunner::new());
    let prim_map = prims::prim_map();

    // Sickos: hahaha YES
    let prog: FuzzProgram = random();

    println!("prog: {}", prog.to_sexp().to_string());

    let args = prog.random_args();
    println!("args: {}", args.to_string());

    prog.interpret(args.clone()).map(|res| {
        println!("interpreted: {}", res.to_string());
    }).map_err(|e| println!("error interpreting: {:?}", e)).ok();

    compile_file(
        &mut allocator,
        runner.clone(),
        opts.clone(),
        &prog.to_sexp().to_string(),
        &mut HashMap::new()
    ).map_err(|e| RunFailure::RunErr(e.0, e.1)).and_then(|compiled| {
        println!("compiled: {}", compiled.to_string());

        run(
            &mut allocator,
            runner.clone(),
            prim_map,
            Rc::new(compiled),
            Rc::new(args)
        )
    }).map(|after_run| {
        println!("result: {}", after_run.to_string());
    }).unwrap_or_else(|e| println!("error compiling or running: {:?}", e));
}
