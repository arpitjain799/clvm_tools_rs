use std::borrow::Borrow;
use std::cell::{Ref, RefCell};
use std::cmp::{Ordering, PartialOrd};
use std::collections::{BTreeMap, HashMap, HashSet};
use std::mem::swap;
use std::rc::Rc;

use lsp_server::{
    ExtractError,
    Request,
    RequestId
};

use lsp_types::{
    Position,
    Range,
    SemanticTokenModifier,
    SemanticTokenType
};

use crate::compiler::clvm::sha256tree_from_atom;
use crate::compiler::compiler::DefaultCompilerOpts;
use crate::compiler::frontend::{
    compile_bodyform,
    compile_helperform
};
use crate::compiler::comptypes::{
    BodyForm,
    CompileForm,
    CompilerOpts,
    HelperForm
};
use crate::compiler::sexp::{
    SExp,
    decode_string,
    parse_sexp
};
use crate::compiler::srcloc::Srcloc;
use crate::compiler::lsp::compopts::LSPCompilerOpts;
use crate::compiler::lsp::parse::{
    ParsedDoc,
    ParseOutput,
    ParseResult,
    grab_scope_doc_range,
    make_simple_ranges,
    recover_scopes
};
use crate::compiler::lsp::patch::{
    split_text,
    stringify_doc
};
use crate::compiler::lsp::semtok::SemanticTokenSortable;

lazy_static! {
    pub static ref TOKEN_TYPES: Vec<SemanticTokenType> = {
        vec![
            SemanticTokenType::PARAMETER,
            SemanticTokenType::VARIABLE,
            SemanticTokenType::FUNCTION,
            SemanticTokenType::MACRO,
            SemanticTokenType::KEYWORD,
            SemanticTokenType::COMMENT,
            SemanticTokenType::STRING,
            SemanticTokenType::NUMBER,
            SemanticTokenType::OPERATOR
        ]
    };

    pub static ref TOKEN_MODIFIERS: Vec<SemanticTokenModifier> = {
        vec![
            SemanticTokenModifier::DEFINITION,
            SemanticTokenModifier::READONLY,
            SemanticTokenModifier::DOCUMENTATION
        ]
    };
}

pub const TK_PARAMETER_IDX: u32 = 0;
pub const TK_VARIABLE_IDX: u32 = 1;
pub const TK_FUNCTION_IDX: u32 = 2;
pub const TK_MACRO_IDX: u32 = 3;
pub const TK_KEYWORD_IDX: u32 = 4;
pub const TK_COMMENT_IDX: u32 = 5;
pub const TK_STRING_IDX: u32 = 6;
pub const TK_NUMBER_IDX: u32 = 7;
pub const TK_OPERATOR_IDX: u32 = 8;

pub const TK_DEFINITION_BIT: u32 = 0;
pub const TK_READONLY_BIT: u32 = 1;
pub const TK_DOCUMENTATION_BIT: u32 = 2;

pub fn cast<R>(req: Request) -> Result<(RequestId, R::Params), ExtractError<Request>>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct DocPosition {
    pub line: u32,
    pub character: u32
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct DocRange {
    pub start: DocPosition,
    pub end: DocPosition
}

#[derive(Clone, Debug)]
pub struct DocPatch {
    pub range: DocRange,
    pub text: String
}

impl DocPosition {
    pub fn from_position(p: &Position) -> Self {
        DocPosition {
            line: p.line,
            character: p.character
        }
    }

    pub fn to_position(&self) -> Position {
        Position {
            line: self.line,
            character: self.character
        }
    }
}

impl DocRange {
    pub fn from_range(r: &Range) -> Self {
        DocRange {
            start: DocPosition::from_position(&r.start),
            end: DocPosition::from_position(&r.end)
        }
    }

    pub fn from_srcloc(l: Srcloc) -> Self {
        let e = l.ending();
        DocRange {
            start: DocPosition {
                line:
                if l.line > 0 { (l.line - 1) as u32 } else { 0 },
                character:
                if l.col > 0 { (l.col - 1) as u32 } else { 0 },
            },
            end: DocPosition {
                line:
                if e.line > 0 { (e.line - 1) as u32 } else { 0 },
                character:
                if e.col > 0 { (e.col - 1) as u32 } else { 0  },
            }
        }
    }

    pub fn to_range(&self) -> Range {
        Range {
            start: self.start.to_position(),
            end: self.end.to_position()
        }
    }

    pub fn overlap(&self, other: &DocRange) -> bool {
        let mut sortable = vec![
            (self.start.clone(), 0),
            (self.end.clone(), 0),
            (other.start.clone(), 1),
            (other.end.clone(), 1)
        ];
        sortable.sort();

        // Not overlapping if both points are on the same side of the other 2
        sortable[0].1 != sortable[1].1
    }
}

#[derive(Debug, Clone)]
pub struct DocData {
    pub text: Vec<Rc<Vec<u8>>>,
}

impl DocData {
    pub fn nth_line_ref(&self, line: usize) -> Option<&Vec<u8>> {
        if line < self.text.len() {
            let borrowed: &Vec<u8> = self.text[line].borrow();
            Some(borrowed)
        } else {
            None
        }
    }

    // Given a position go back one character, returning the character
    // and the new position if they exist.
    pub fn get_prev_position(&self, position: &Position) -> Option<(u8, Position)> {
        if position.character == 0 && position.line > 0 && ((position.line - 1) as usize) < self.text.len() {
            let nextline = position.line - 1;
            self.get_prev_position(&Position {
                line: nextline,
                character: self.text[nextline as usize].len() as u32
            })
        } else {
            self.nth_line_ref(position.line as usize).and_then(|line| {
                if position.character > 0 && (position.character as usize) <= line.len() {
                    let prev_char = position.character - 1;
                    let the_char = line[prev_char as usize];
                    Some((the_char, Position {
                        line: position.line,
                        character: prev_char
                    }))
                } else {
                    None
                }
            })
        }
    }

    // Given a position, get the pointed-to character.
    pub fn get_at_position(&self, position: &Position) -> Option<u8> {
        self.nth_line_ref(position.line as usize).and_then(|line| {
            if (position.character as usize) < line.len() {
                Some(line[position.character as usize])
            } else {
                None
            }
        })
    }
}

#[derive(Debug, Clone)]
struct HelperWithDocRange {
    pub loc: DocRange,
    pub h: HelperForm
}

impl PartialEq for HelperWithDocRange {
    fn eq(&self, other: &Self) -> bool {
        self.loc == other.loc
    }
}

impl PartialOrd for HelperWithDocRange {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.loc.cmp(&other.loc))
    }
}

impl Eq for HelperWithDocRange { }
impl Ord for HelperWithDocRange {
    fn cmp(&self, other: &Self) -> Ordering {
        self.loc.cmp(&other.loc)
    }
}

pub struct LSPServiceProvider {
    // Let document collection be sharable due to the need to capture it for
    // use in compiler opts.
    pub document_collection: Rc<RefCell<HashMap<String, DocData>>>,

    // These aren't shared.
    pub parsed_documents: HashMap<String, ParsedDoc>,
    pub goto_defs: HashMap<String, BTreeMap<SemanticTokenSortable, Srcloc>>,
    pub pending_patches: HashMap<String, Vec<DocPatch>>,
}

pub struct ReparsedHelper {
    pub hash: Vec<u8>,
    pub parsed: HelperForm
}

pub struct ReparsedModule {
    pub args: Rc<SExp>,
    pub helpers: Vec<ReparsedHelper>,
    pub exp: Rc<BodyForm>
}

pub fn reparse_subset(
    opts: Rc<dyn CompilerOpts>,
    doc: &[Rc<Vec<u8>>],
    uristring: &String,
    simple_ranges: &Vec<DocRange>,
    compiled: &CompileForm,
    used_hashes: &HashSet<Vec<u8>>
) -> ReparsedModule {
    let mut result = ReparsedModule {
        args: compiled.args.clone(),
        helpers: Vec::new(),
        exp: compiled.exp.clone()
    };

    // if it's a module, we can patch the prefix in, otherwise make a (mod ()
    // prefix for it.
    // We can take the last phrase and if it's not a helper, we can use it as
    // the end of the document.
    let mut took_args = false;

    // Capture the simple ranges, then check each one's hash
    // if the hash isn't present in the helpers we have, we need to run the
    // frontend on it.
    for (i, r) in simple_ranges.iter().enumerate() {
        let text = grab_scope_doc_range(doc, &r, false);
        eprintln!("helper text {}", decode_string(&text));
        let hash = sha256tree_from_atom(&text);
        if !used_hashes.contains(&hash) {
            if let Ok(parsed) =
                parse_sexp(Srcloc::new(Rc::new(uristring.clone()), (r.start.line + 1) as usize, (r.start.character + 1) as usize), text.iter().copied())
            {
                if let Ok(h) = compile_helperform(opts.clone(), parsed[0].clone()) {
                    if let Some(helper) = h.as_ref() {
                        result.helpers.push(ReparsedHelper {
                            hash,
                            parsed: helper.clone()
                        });
                    } else {
                        if i == simple_ranges.len() - 1 {
                            if let Ok(body) =
                                compile_bodyform(parsed[0].clone())
                            {
                                result.exp = Rc::new(body);
                            }
                        } else if !took_args {
                            result.args = parsed[0].clone();
                            took_args = true;
                        }
                    };
                }
            }
        }
    }

    result
}

impl LSPServiceProvider {
    pub fn with_doc_and_parsed<F,G>(&mut self, uristring: &String, f: F) -> Option<G>
    where
        F: FnOnce(&DocData, &ParseOutput) -> Option<G>
    {
        self.ensure_parsed_document(uristring);
        if let (Some(d), Some(p)) = (self.get_doc(uristring), self.get_parsed(uristring)) {
            if let ParseResult::Completed(o) = p.result {
                f(&d, &o)
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn get_doc(&self, uristring: &String) -> Option<DocData> {
        let cell: &RefCell<HashMap<String, DocData>> = self.document_collection.borrow();
        let coll: Ref<HashMap<String, DocData>> = cell.borrow();
        (&coll).get(uristring).map(|x| x.clone())
    }

    pub fn get_parsed(&self, uristring: &String) -> Option<ParsedDoc> {
        self.parsed_documents.get(uristring).cloned()
    }

    pub fn save_doc(&mut self, uristring: String, dd: DocData) {
        let cell: &RefCell<HashMap<String, DocData>> = self.document_collection.borrow();
        self.parsed_documents.remove(&uristring);
        cell.replace_with(|coll| {
            let mut repl = HashMap::new();
            swap(&mut repl, coll);
            repl.insert(uristring.clone(), dd);
            repl
        });
    }

    fn save_parse(&mut self, uristring: String, p: ParsedDoc) {
        self.parsed_documents.insert(uristring, p);
    }

    pub fn ensure_parsed_document<'a>(
        &mut self,
        uristring: &String
    ) {
        let opts = Rc::new(LSPCompilerOpts::new(uristring.clone(), self.document_collection.clone()));

        if let Some(doc) = self.get_doc(uristring) {
            let prior_parse = self.parsed_documents.get(uristring);
            if let (Some(pending_patches), Some(ParseResult::Completed(output))) =
                ( self.pending_patches.get(uristring),
                  prior_parse.as_ref().map(|d| &d.result)
                )
            {
                let mut patches: Vec<DocPatch> = pending_patches.iter().cloned().collect();
                self.pending_patches.remove(uristring);

                let ranges = make_simple_ranges(&doc.text);
                let new_helpers = reparse_subset(
                    opts.clone(),
                    &doc.text,
                    uristring,
                    &ranges,
                    &output.compiled,
                    &output.hashes
                );

                let mut new_hashes = output.hashes.clone();
                for new_helper in new_helpers.helpers.iter() {
                    new_hashes.insert(new_helper.hash.clone());
                }

                let extracted_helpers: Vec<HelperForm> =
                    new_helpers.helpers.iter().map(|h| h.parsed.clone()).collect();
                let mut new_compile =
                    output.compiled.replace_helpers(&extracted_helpers);
                new_compile.args = new_helpers.args.clone();
                new_compile.exp = new_helpers.exp.clone();

                self.save_parse(uristring.clone(), ParsedDoc {
                    result: ParseResult::Completed(ParseOutput {
                        compiled: new_compile.clone(),
                        simple_ranges: ranges,
                        hashes: new_hashes,
                        scopes: recover_scopes(uristring, &doc.text, &new_compile)
                    })
                });
            } else if prior_parse.is_none() {
                let parsed = ParsedDoc::new(opts, uristring, &doc.text);
                self.save_parse(uristring.clone(), parsed);
            }
        }
    }

    pub fn new() -> Self {
        LSPServiceProvider {
            document_collection: Rc::new(RefCell::new(HashMap::new())),

            parsed_documents: HashMap::new(),
            goto_defs: HashMap::new(),
            pending_patches: HashMap::new()
        }
    }

    pub fn get_file(&self, filename: &String) -> Result<String, String> {
        self.get_doc(filename).map(|d| stringify_doc(&d.text)).unwrap_or_else(|| Err(format!("don't have file {}", filename)))
    }
}
