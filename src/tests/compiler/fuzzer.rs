use rand::prelude::*;
use std::rc::Rc;

use crate::compiler::comptypes::{CompileErr, CompileForm};
use crate::compiler::fuzzer::{CollectArgumentStructure, CollectProgramStructure, ModernSexpBuilder};
use crate::compiler::sexp::{parse_sexp, SExp};
use crate::compiler::srcloc::Srcloc;
use crate::fuzzing::fuzzrng::FuzzPseudoRng;

fn produce_fuzz_program(b: &[u8]) -> CompileForm {
    let mut rng = FuzzPseudoRng::new(b);
    let mut cps: CollectProgramStructure = rng.gen();
    cps.to_program()
}

fn produce_fuzz_args(b: &[u8], prototype: Rc<SExp>) -> Result<Rc<SExp>,CompileErr> {
    let mut rng = FuzzPseudoRng::new(b);
    let mut sexp_builder: ModernSexpBuilder = Default::default();
    let mut cps: CollectArgumentStructure = rng.gen();
    eprintln!("cps {cps:?}");
    cps.to_sexp(&mut sexp_builder, prototype)
}

#[test]
fn test_simple_fuzzer_output_1() {
    let input: &[u8] = &[
        0x00, 0x00, 0x00, 0xaf, 0xe8, 0x20, 0xb7, 0x82, 0x07, 0x29, 0xae, 0x0f, 0x22, 0xb3, 0xf5,
        0x5b,
    ];
    let cf = produce_fuzz_program(input);
    assert_eq!(
        cf.to_sexp().to_string(),
        "(E (defmacro helper_0 C (18 (q) (q))) (q))"
    );
}

#[test]
fn test_simple_fuzzer_output_2() {
    let input: &[u8] = &[
        0x00, 0x00, 0x00, 0xaf, 0xe8, 0x20, 0xb7, 0x82, 0x07, 0x3f, 0x79, 0xaa, 0x72, 0xb3, 0xf5,
        0x5b,
    ];
    let cf = produce_fuzz_program(input);
    assert_eq!(
        cf.to_sexp().to_string(),
        "(E (defmacro helper_0 C (let ((A (18 (q) (q)))) (18 (q) (q)))) (q))"
    );
}

#[test]
fn test_simple_fuzzer_output_3() {
    let input: &[u8] = &[
        0x00, 0x00, 0x00, 0x8c, 0x66, 0x36, 0xfd, 0xb3, 0x5a, 0x80, 0x9d, 0x45, 0x9c, 0x0e, 0x91,
        0x79,
    ];
    let cf = produce_fuzz_program(input);
    assert_eq!(cf.to_sexp().to_string(), "(A (18 (let ((B A) (D A)) A) A))");
}

#[test]
fn test_argument_structure_builder_smoke() {
    let input: &[u8] = &[];
    let parsed = parse_sexp(Srcloc::start("*args*"), "A".bytes()).expect("should parse");
    assert_eq!(
        produce_fuzz_args(input, parsed[0].clone()).expect("should work").to_string(),
        "()"
    );
}

#[test]
fn test_argument_structure_builder_single_atom() {
    let input: &[u8] = &[0x85,0x01,0x04];
    let parsed = parse_sexp(Srcloc::start("*args*"), "A".bytes()).expect("should parse");
    let res = produce_fuzz_args(input, parsed[0].clone()).expect("should work").to_string();

    todo!();
    assert_eq!(
        res,
        "()"
    );
}
