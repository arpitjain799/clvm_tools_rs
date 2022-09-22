use std::borrow::Borrow;
use std::cell::{Ref, RefCell};
use std::collections::HashMap;
use std::path::Path;
use std::rc::Rc;

use clvmr::allocator::Allocator;

use crate::classic::clvm_tools::stages::stage_0::TRunProgram;
use crate::compiler::compiler::{
    compile_pre_forms, create_prim_map, KNOWN_DIALECTS, STANDARD_MACROS,
};
use crate::compiler::comptypes::{CompileErr, CompilerOpts, PrimaryCodegen};
use crate::compiler::lsp::patch::{compute_comment_lines, split_text, stringify_doc};
use crate::compiler::lsp::types::{DocData, IFileReader};
use crate::compiler::sexp::{decode_string, SExp};
use crate::compiler::srcloc::Srcloc;

#[derive(Clone)]
pub struct LSPCompilerOpts {
    pub fs: Rc<dyn IFileReader>,
    pub include_dirs: Vec<String>,
    pub filename: String,
    pub compiler: Option<PrimaryCodegen>,
    pub in_defun: bool,
    pub stdenv: bool,
    pub optimize: bool,
    pub frontend_opt: bool,
    pub start_env: Option<Rc<SExp>>,
    pub prim_map: Rc<HashMap<Vec<u8>, Rc<SExp>>>,

    lsp: Rc<RefCell<HashMap<String, DocData>>>,

    known_dialects: Rc<HashMap<String, String>>,
}

impl CompilerOpts for LSPCompilerOpts {
    fn filename(&self) -> String {
        self.filename.clone()
    }
    fn compiler(&self) -> Option<PrimaryCodegen> {
        self.compiler.clone()
    }
    fn in_defun(&self) -> bool {
        self.in_defun
    }
    fn stdenv(&self) -> bool {
        self.stdenv
    }
    fn optimize(&self) -> bool {
        self.optimize
    }
    fn frontend_opt(&self) -> bool {
        self.frontend_opt
    }
    fn frontend_check_live(&self) -> bool {
        false
    }
    fn start_env(&self) -> Option<Rc<SExp>> {
        self.start_env.clone()
    }
    fn prim_map(&self) -> Rc<HashMap<Vec<u8>, Rc<SExp>>> {
        self.prim_map.clone()
    }

    fn set_search_paths(&self, dirs: &[String]) -> Rc<dyn CompilerOpts> {
        let mut copy = self.clone();
        copy.include_dirs = dirs.to_owned();
        Rc::new(copy)
    }
    fn set_in_defun(&self, new_in_defun: bool) -> Rc<dyn CompilerOpts> {
        let mut copy = self.clone();
        copy.in_defun = new_in_defun;
        Rc::new(copy)
    }
    fn set_stdenv(&self, new_stdenv: bool) -> Rc<dyn CompilerOpts> {
        let mut copy = self.clone();
        copy.stdenv = new_stdenv;
        Rc::new(copy)
    }
    fn set_optimize(&self, optimize: bool) -> Rc<dyn CompilerOpts> {
        let mut copy = self.clone();
        copy.optimize = optimize;
        Rc::new(copy)
    }
    fn set_frontend_opt(&self, optimize: bool) -> Rc<dyn CompilerOpts> {
        let mut copy = self.clone();
        copy.frontend_opt = optimize;
        Rc::new(copy)
    }
    fn set_compiler(&self, new_compiler: PrimaryCodegen) -> Rc<dyn CompilerOpts> {
        let mut copy = self.clone();
        copy.compiler = Some(new_compiler);
        Rc::new(copy)
    }
    fn set_start_env(&self, start_env: Option<Rc<SExp>>) -> Rc<dyn CompilerOpts> {
        let mut copy = self.clone();
        copy.start_env = start_env;
        Rc::new(copy)
    }

    fn read_new_file(
        &self,
        inc_from: String,
        filename: String,
    ) -> Result<(String, String), CompileErr> {
        if filename == "*macros*" {
            return Ok((filename, STANDARD_MACROS.clone()));
        } else if let Some(content) = self.known_dialects.get(&filename) {
            return Ok((filename, content.to_string()));
        }

        for dir in self.include_dirs.iter() {
            let p = format!("{}/{}", dir, filename);
            match self.get_file(&p) {
                Err(_e) => {
                    continue;
                }
                Ok(content) => {
                    return stringify_doc(&content.text)
                        .map(|s| (filename, s))
                        .map_err(|x| CompileErr(Srcloc::start(&p), x));
                }
            }
        }

        Err(CompileErr(
            Srcloc::start(&inc_from),
            format!("could not find {} to include", filename),
        ))
    }

    fn compile_program(
        &self,
        allocator: &mut Allocator,
        runner: Rc<dyn TRunProgram>,
        sexp: Rc<SExp>,
        symbol_table: &mut HashMap<String, String>,
    ) -> Result<SExp, CompileErr> {
        let me = Rc::new(self.clone());
        eprintln!("compile_program {}", sexp);
        compile_pre_forms(allocator, runner, me, &[sexp], symbol_table)
    }
}

pub fn get_file_content(
    reader: Rc<dyn IFileReader>,
    include_paths: &[String],
    name: &str,
) -> Result<(String, DocData), String> {
    for find_path in include_paths.iter() {
        if let Some(try_path) = Path::new(&find_path).join(name).to_str() {
            if let Ok(filedata) = reader.read(try_path) {
                let doc_text = split_text(&decode_string(&filedata));
                let comments = compute_comment_lines(&doc_text);

                return Ok((
                    try_path.to_string(),
                    DocData {
                        text: doc_text,
                        version: -1,
                        comments,
                    },
                ));
            }
        }
    }
    Err(format!("don't have {} to open", name))
}

impl LSPCompilerOpts {
    pub fn new(
        fs: Rc<dyn IFileReader>,
        filename: &str,
        paths: &[String],
        docs: Rc<RefCell<HashMap<String, DocData>>>,
    ) -> Self {
        LSPCompilerOpts {
            fs,
            include_dirs: paths.to_owned(),
            filename: filename.to_owned(),
            compiler: None,
            in_defun: false,
            stdenv: true,
            optimize: false,
            frontend_opt: false,
            start_env: None,
            prim_map: create_prim_map(),
            lsp: docs,
            known_dialects: Rc::new(KNOWN_DIALECTS.clone()),
        }
    }

    fn get_file(&self, name: &str) -> Result<DocData, String> {
        let cell: &RefCell<HashMap<String, DocData>> = self.lsp.borrow();
        let coll: Ref<HashMap<String, DocData>> = cell.borrow();
        coll.get(name).map(|x| Ok(x.clone())).unwrap_or_else(|| {
            get_file_content(self.fs.clone(), &self.include_dirs, name).map(|x| x.1)
        })
    }
}
