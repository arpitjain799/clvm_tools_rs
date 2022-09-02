use std::borrow::Borrow;
use std::collections::HashSet;
use std::rc::Rc;

use lsp_types::Position;

use crate::compiler::comptypes::{
    CompileErr,
    CompileForm,
    CompilerOpts,
    HelperForm
};
use crate::compiler::frontend::frontend;
use crate::compiler::sexp::{SExp, decode_string, parse_sexp};
use crate::compiler::srcloc::Srcloc;
use crate::compiler::lsp::types::DocData;

#[derive(Debug, Clone)]
pub enum ScopeKind {
    Module,
    Macro,
    Function,
    Let
}

#[derive(Debug, Clone)]
pub struct ParseScope {
    pub region: Srcloc,
    pub kind: ScopeKind,
    pub variables: HashSet<SExp>,
    pub functions: HashSet<SExp>,
    pub containing: Vec<ParseScope>
}

#[derive(Debug, Clone)]
pub struct ParseOutput {
    pub compiled: CompileForm,
    pub scopes: ParseScope
}

#[derive(Debug, Clone)]
pub enum ParseResult {
    WithError(CompileErr),
    Completed(ParseOutput)
}

#[derive(Debug, Clone)]
pub struct ParsedDoc {
    pub parses: Vec<u32>,
    pub result: ParseResult,
}

pub struct DocVecByteIter<'a> {
    line: usize,
    offs: usize,
    target: &'a [Rc<Vec<u8>>]
}

impl<'a> Iterator for DocVecByteIter<'a> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.line >= self.target.len() {
                return None;
            } else if self.offs >= self.target[self.line].len() {
                self.line += 1;
                self.offs = 0;
                return Some(b'\n');
            } else {
                let res = self.target[self.line][self.offs];
                self.offs += 1;
                return Some(res);
            }
        }
    }
}

impl<'a> DocVecByteIter<'a> {
    pub fn new(target: &'a [Rc<Vec<u8>>]) -> Self {
        DocVecByteIter {
            line: 0,
            offs: 0,
            target
        }
    }
}

pub fn find_ident(line: Rc<Vec<u8>>, char_at: u32) -> Option<Vec<u8>> {
    let ca_size = char_at as usize;
    let ll = line.len();

    if ca_size > ll {
        return None;
    }

    let borrowed: &Vec<u8> = line.borrow();
    let mut lb = ca_size - 1;
    let mut ub = ca_size;
    while lb > 0 && borrowed[lb-1].is_ascii_alphabetic() {
        lb -= 1;
    }
    while ub < borrowed.len() && borrowed[ub].is_ascii_alphanumeric() {
        ub += 1;
    }

    let ident_vec: Vec<u8> =
        borrowed[lb..].iter().
        take(ub - lb).copied().collect();

    Some(ident_vec)
}

pub fn get_positional_text(lines: &DocData, position: &Position) -> Option<Vec<u8>> {
    let pl = position.line as usize;
    if pl < lines.text.len() {
        if position.character == 0 {
            None
        } else {
            let line = lines.text[pl].clone();
            find_ident(line.clone(), position.character)
        }
    } else {
        None
    }
}

pub fn is_identifier(v: &Vec<u8>) -> bool {
    v.iter().all(|x| !(*x == b'(' || *x == b')' || x.is_ascii_whitespace()))
}

pub fn find_scope_stack(
    out_scopes: &mut Vec<ParseScope>,
    scope: &ParseScope,
    position: &Srcloc
) {
    if scope.region.overlap(&position) {
        for s in scope.containing.iter() {
            find_scope_stack(out_scopes, s, position);
        }
        out_scopes.push(scope.clone());
    }
}

fn grab_scope_range(text: &[Rc<Vec<u8>>], loc: Srcloc) -> String {
    let eloc = loc.ending();
    let mut res = Vec::new();

    if (loc.line - 1) >= text.len() {
        return "".to_string();
    }

    // First line
    let tline = text[loc.line - 1].clone();
    let text_borrowed: &Vec<u8> = tline.borrow();

    if eloc.line == loc.line {
        // Only line
        for ch in text_borrowed.iter().take(eloc.col).skip(loc.col - 1) {
            res.push(*ch);
        }
    } else {
        for ch in text_borrowed.iter().skip(loc.col - 1) {
            res.push(*ch);
        }
    }

    for ch in text_borrowed.iter() {
        res.push(*ch);
    }

    // Inside lines.
    let end_line =
        if eloc.line - 1 > text.len() {
            text.len()
        } else {
            eloc.line - 1
        };

    eprintln!("loc.line {} end_line {}", loc.line, end_line);
    if end_line > 0 && loc.line < end_line - 1 {
        for l in text[loc.line..end_line - 1].iter() {
            res.push(b'\n');
            let iline = l.clone();
            let il: &Vec<u8> = iline.borrow();
            for ch in il.iter() {
                res.push(*ch);
            }
        }
    }

    let eline = text[end_line].clone();
    let end_borrowed: &Vec<u8> = eline.borrow();

    res.push(b'\n');
    for ch in end_borrowed.iter().take(eloc.col - 1) {
        res.push(*ch);
    }

    decode_string(&res)
}

fn make_arg_set(set: &mut HashSet<SExp>, args: Rc<SExp>) {
    match args.borrow() {
        SExp::Atom(l,a) => {
            set.insert(SExp::Atom(l.clone(),a.clone()));
        },
        SExp::Cons(l,a,b) => {
            make_arg_set(set, a.clone());
            make_arg_set(set, b.clone());
        },
        _ => { }
    }
}

fn make_helper_scope(h: &HelperForm) -> Option<ParseScope> {
    let loc = h.loc();
    let eloc = loc.ending();

    let mut kind = None;
    let mut args = HashSet::new();

    match h {
        HelperForm::Defun(i,d) => {
            kind = Some(ScopeKind::Function);
            make_arg_set(&mut args, d.args.clone());
        },
        HelperForm::Defmacro(m) => {
            kind = Some(ScopeKind::Macro);
            make_arg_set(&mut args, m.args.clone());
        },
        _ => { }
    }

    kind.map(|k| {
        ParseScope {
            kind: k,
            region: loc,
            variables: args,
            functions: HashSet::new(),
            containing: Vec::new()
        }
    })
}

fn recover_scopes(ourfile: &String, text: &[Rc<Vec<u8>>], fe: &CompileForm) -> ParseScope {
    let mut toplevel_args = HashSet::new();
    let mut toplevel_funs = HashSet::new();
    let mut contained = Vec::new();

    make_arg_set(&mut toplevel_args, fe.args.clone());

    for h in fe.helpers.iter() {
        eprintln!("helper {} has this range in the code: {}", h.to_sexp().to_string(), grab_scope_range(text, h.loc()));

        match h {
            HelperForm::Defun(i,d) => {
                toplevel_funs.insert(SExp::Atom(d.loc.clone(), d.name.clone()));
            },
            HelperForm::Defmacro(m) => {
                toplevel_funs.insert(SExp::Atom(m.loc.clone(), m.name.clone()));
            },
            HelperForm::Defconstant(l,n,c) => {
                toplevel_args.insert(SExp::Atom(l.clone(), n.clone()));
            }
        }

        let f = h.loc().file.clone();
        let filename: &String = f.borrow();
        if filename == ourfile {
            if let Some(scope) = make_helper_scope(h) {
                contained.push(scope);
            }
        }
    }

    ParseScope {
        kind: ScopeKind::Module,
        region: Srcloc::start(ourfile).ext(
            &Srcloc::new(Rc::new(ourfile.clone()), text.len() + 1, 0)
        ),
        variables: toplevel_args,
        functions: toplevel_funs,
        containing: contained
    }
}

impl ParsedDoc {
    pub fn empty() -> Self {
        ParsedDoc {
            parses: vec![],
            result: ParseResult::WithError(CompileErr(Srcloc::start(&"*none*".to_string()), "no file".to_string()))
        }
    }

    pub fn new(opts: Rc<dyn CompilerOpts>, file: &String, srctext: &[Rc<Vec<u8>>]) -> Self {
        let srcloc = Srcloc::start(file);
        parse_sexp(srcloc, DocVecByteIter::new(srctext)).
            map_err(|e| { CompileErr(e.0.clone(), "parse error".to_string()) }).
            map(|parsed| {
                frontend(opts.clone(), &parsed).as_ref().map(|fe| {
                    let parsed = ParseOutput {
                        compiled: fe.clone(),
                        scopes: recover_scopes(file, srctext, fe)
                    };
                    ParsedDoc {
                        parses: Vec::new(),
                        result: ParseResult::Completed(parsed)
                    }
                }).unwrap_or_else(|e| {
                    ParsedDoc {
                        parses: Vec::new(),
                        result: ParseResult::WithError(e.clone())
                    }
                })
            }).unwrap_or_else(|e| {
                ParsedDoc {
                    parses: Vec::new(),
                    result: ParseResult::WithError(e)
                }
            })
    }
}
