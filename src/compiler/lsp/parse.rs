use std::borrow::Borrow;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use lsp_types::Position;

use crate::compiler::comptypes::{BodyForm, CompileErr, CompileForm, HelperForm};
use crate::compiler::lsp::types::{DocData, DocPosition, DocRange};
use crate::compiler::sexp::SExp;
use crate::compiler::srcloc::Srcloc;

#[derive(Debug, Clone)]
pub enum ScopeKind {
    Module,
    Macro,
    Function,
    Let,
}

#[derive(Debug, Clone)]
pub struct ParseScope {
    pub region: Srcloc,
    pub kind: ScopeKind,
    pub variables: HashSet<SExp>,
    pub functions: HashSet<SExp>,
    pub containing: Vec<ParseScope>,
}

#[derive(Debug, Clone)]
pub struct ParsedDoc {
    pub compiled: CompileForm,
    pub errors: HashMap<Vec<u8>, CompileErr>,
    pub scopes: ParseScope,
    pub name_to_hash: HashMap<Vec<u8>, Vec<u8>>,
    pub hashes: HashSet<Vec<u8>>,
    pub includes: HashMap<Vec<u8>, Vec<u8>>,
}

impl ParsedDoc {
    pub fn new(startloc: Srcloc) -> Self {
        let nil = SExp::Nil(startloc.clone());
        ParsedDoc {
            hashes: HashSet::new(),
            name_to_hash: HashMap::new(),
            includes: HashMap::new(),
            errors: HashMap::new(),
            scopes: ParseScope {
                region: startloc.clone(),
                kind: ScopeKind::Module,
                variables: HashSet::new(),
                functions: HashSet::new(),
                containing: vec![],
            },
            compiled: CompileForm {
                loc: startloc,
                args: Rc::new(nil.clone()),
                helpers: vec![],
                exp: Rc::new(BodyForm::Quoted(nil)),
            },
        }
    }
}

pub struct DocVecByteIter<'a> {
    line: usize,
    offs: usize,
    target: &'a [Rc<Vec<u8>>],
}

impl<'a> Iterator for DocVecByteIter<'a> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        if self.line >= self.target.len() {
            None
        } else if self.offs >= self.target[self.line].len() {
            self.line += 1;
            self.offs = 0;
            Some(b'\n')
        } else {
            let res = self.target[self.line][self.offs];
            self.offs += 1;
            Some(res)
        }
    }
}

impl<'a> DocVecByteIter<'a> {
    pub fn new(target: &'a [Rc<Vec<u8>>]) -> Self {
        DocVecByteIter {
            line: 0,
            offs: 0,
            target,
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
    while lb > 0 && borrowed[lb - 1].is_ascii_alphabetic() {
        lb -= 1;
    }
    while ub < borrowed.len() && borrowed[ub].is_ascii_alphanumeric() {
        ub += 1;
    }

    let ident_vec: Vec<u8> = borrowed[lb..].iter().take(ub - lb).copied().collect();

    Some(ident_vec)
}

// A position points to "first in list" if reversing past all alphanumerics
// then all spaces yields a character other than '('.
pub fn is_first_in_list(lines: &DocData, position: &Position) -> bool {
    let mut current_char = b' ';
    let mut pos_walk = *position;

    // Reverse past this identifier's start.
    while let Some((ch, p)) = lines.get_prev_position(&pos_walk) {
        current_char = ch;

        if ch.is_ascii_whitespace() || ch == b'(' || ch == b')' {
            break;
        }

        pos_walk = p;
    }

    // We ran into ( early.
    if current_char == b'(' {
        return true;
    }

    // Reverse past spaces.
    while let Some((ch, p)) = lines.get_prev_position(&pos_walk) {
        current_char = ch;

        if !ch.is_ascii_whitespace() {
            break;
        }

        pos_walk = p;
    }

    current_char == b'('
}

pub fn get_positional_text(lines: &DocData, position: &Position) -> Option<Vec<u8>> {
    let pl = position.line as usize;
    if pl < lines.text.len() {
        if position.character == 0 {
            None
        } else {
            let line = lines.text[pl].clone();
            find_ident(line, position.character)
        }
    } else {
        None
    }
}

pub fn is_identifier(v: &[u8]) -> bool {
    v.iter()
        .all(|x| !(*x == b'(' || *x == b')' || x.is_ascii_whitespace()))
}

pub fn find_scope_stack(out_scopes: &mut Vec<ParseScope>, scope: &ParseScope, position: &Srcloc) {
    if scope.region.overlap(position) {
        for s in scope.containing.iter() {
            find_scope_stack(out_scopes, s, position);
        }
        out_scopes.push(scope.clone());
    }
}

pub fn grab_scope_doc_range(
    text: &[Rc<Vec<u8>>],
    range: &DocRange,
    space_for_range: bool,
) -> Vec<u8> {
    let mut res = Vec::new();

    let loc = &range.start;
    let eloc = &range.end;

    if space_for_range {
        res.append(&mut vec![b'\n'; loc.line as usize]);
        res.append(&mut vec![b' '; loc.character as usize]);
    }

    // First line
    let tline = text[loc.line as usize].clone();
    let text_borrowed: &Vec<u8> = tline.borrow();

    if eloc.line == loc.line {
        // Only line
        for ch in text_borrowed
            .iter()
            .take(eloc.character as usize)
            .skip(loc.character as usize)
        {
            res.push(*ch);
        }

        return res;
    }

    for ch in text_borrowed.iter().skip(loc.character as usize) {
        res.push(*ch);
    }

    // Inside lines.
    let end_line = if (eloc.line as usize) > text.len() {
        text.len()
    } else {
        eloc.line as usize
    };

    for l in text.iter().take(end_line).skip((loc.line + 1) as usize) {
        res.push(b'\n');
        let iline = l.clone();
        let il: &Vec<u8> = iline.borrow();
        for ch in il.iter() {
            res.push(*ch);
        }
    }

    let eline = if end_line < text.len() {
        text[end_line].clone()
    } else {
        Rc::new(vec![])
    };

    let end_borrowed: &Vec<u8> = eline.borrow();

    res.push(b'\n');
    for ch in end_borrowed.iter().take(eloc.character as usize) {
        res.push(*ch);
    }

    res
}

fn make_arg_set(set: &mut HashSet<SExp>, args: Rc<SExp>) {
    match args.borrow() {
        SExp::Atom(l, a) => {
            set.insert(SExp::Atom(l.clone(), a.clone()));
        }
        SExp::Cons(_, a, b) => {
            make_arg_set(set, a.clone());
            make_arg_set(set, b.clone());
        }
        _ => {}
    }
}

fn make_helper_scope(h: &HelperForm) -> Option<ParseScope> {
    let loc = h.loc();

    let mut kind = None;
    let mut args = HashSet::new();

    match h {
        HelperForm::Defun(_, d) => {
            kind = Some(ScopeKind::Function);
            make_arg_set(&mut args, d.args.clone());
        }
        HelperForm::Defmacro(m) => {
            kind = Some(ScopeKind::Macro);
            make_arg_set(&mut args, m.args.clone());
        }
        _ => {}
    }

    kind.map(|k| ParseScope {
        kind: k,
        region: loc,
        variables: args,
        functions: HashSet::new(),
        containing: Vec::new(),
    })
}

pub fn recover_scopes(ourfile: &str, text: &[Rc<Vec<u8>>], fe: &CompileForm) -> ParseScope {
    let mut toplevel_args = HashSet::new();
    let mut toplevel_funs = HashSet::new();
    let mut contained = Vec::new();

    make_arg_set(&mut toplevel_args, fe.args.clone());

    for h in fe.helpers.iter() {
        match h {
            HelperForm::Defun(_, d) => {
                toplevel_funs.insert(SExp::Atom(d.loc.clone(), d.name.clone()));
            }
            HelperForm::Defmacro(m) => {
                toplevel_funs.insert(SExp::Atom(m.loc.clone(), m.name.clone()));
            }
            HelperForm::Defconstant(l, n, _) => {
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
        region: Srcloc::start(ourfile).ext(&Srcloc::new(
            Rc::new(ourfile.to_string()),
            text.len() + 1,
            0,
        )),
        variables: toplevel_args,
        functions: toplevel_funs,
        containing: contained,
    }
}

pub fn make_simple_ranges(srctext: &[Rc<Vec<u8>>]) -> Vec<DocRange> {
    let mut ranges = Vec::new();
    let mut start = None;
    let mut level = 0;
    let mut line = 0;
    let mut character = 0;

    for i in DocVecByteIter::new(srctext) {
        if i == b'(' {
            if level == 1 && start.is_none() {
                start = Some(DocPosition { line, character });
            }

            level += 1;
        } else if i == b')' {
            // We expect to contain only one toplevel list, so other ends
            // are probably a misparse.
            if level > 0 {
                level -= 1;

                if level == 1 {
                    if let Some(s) = start.clone() {
                        ranges.push(DocRange {
                            start: s,
                            end: DocPosition {
                                line,
                                character: character + 1,
                            },
                        });
                        start = None;
                    }
                }
            }
        }

        if i == b'\n' {
            line += 1;
            character = 0;
        } else {
            character += 1;
        }
    }

    ranges
}
