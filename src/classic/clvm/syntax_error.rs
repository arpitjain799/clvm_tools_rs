use std::io;
use std::{error::Error, fmt};

use clvmr::allocator::NodePtr;
use clvmr::reduction::EvalErr;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SyntaxErr {
    pub msg: String,
}

impl SyntaxErr {
    pub fn new(s: String) -> Self { SyntaxErr { msg: s } }

    pub fn to_eval_err(&self, n: NodePtr) -> EvalErr {
        EvalErr(n, self.msg.clone())
    }
}

impl Error for SyntaxErr {}

impl fmt::Display for SyntaxErr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.msg)
    }
}

impl From<SyntaxErr> for io::Error {
    fn from(err: SyntaxErr) -> Self {
        Self::new(io::ErrorKind::Other, err.msg)
    }
}
