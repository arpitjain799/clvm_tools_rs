#![feature(exclusive_range_pattern)]

#[macro_use]
extern crate lazy_static;

#[macro_use]
extern crate derivative;

#[macro_use]
extern crate indoc;

#[macro_use]
extern crate do_notation;

#[macro_use]
#[cfg(all(not(test), not(target_family = "wasm"), feature = "extension-module"))]
extern crate pyo3;

extern crate tempfile;

extern crate clvmr as clvm_rs;

pub mod util;

pub mod classic;
pub mod compiler;
#[cfg(any(test, feature="fuzzer"))]
pub mod fuzzing;

// Python impl
#[cfg(all(not(test), not(target_family = "wasm"), feature = "extension-module"))]
mod py;

#[cfg(test)]
mod tests;
