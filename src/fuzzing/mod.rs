use num_bigint::ToBigInt;
use num_traits::ToPrimitive;
use rand::prelude::*;
use rand_chacha::ChaCha8Rng;

use crate::compiler::sexp::random_atom_name;
use crate::util::number_from_u8;

pub mod fuzzrng;

// Note: this is for use by things that test code downstream of the fuzzer using
// a standard rng.  This computes a random seed that may be used to allow the
// infrastructure downstream of the fuzzer to produce randomly generated programs
// without the entropy slice that the fuzzer provides.
//
// This is not used in any way in the rest of the fuzzer infrastructure but is
// docked here mostly because i received pushback about having it live elsewhere
// so it now lives in a place whose presence in the final build artifact is
// goverened by the "fuzzer" feature and so comes and goes with any code that
// might have reason to use it.  Its presence here does not indicate that it is
// in the path of the fuzzer.
pub fn make_random_u64_seed() -> u64 {
    let mut rng = ChaCha8Rng::from_entropy();
    let random_seed = random_atom_name(&mut rng, 10);
    let random_seed_as_bigint =
        number_from_u8(&random_seed) & 0xffffffffffff_u64.to_bigint().unwrap();
    random_seed_as_bigint.to_u64().unwrap()
}
