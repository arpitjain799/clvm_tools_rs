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
    CompileErr,
    CompileForm,
    CompilerOpts,
    HelperForm
};
use crate::compiler::sexp::{
    SExp,
    decode_string,
    enlist,
    parse_sexp
};
use crate::compiler::srcloc::Srcloc;
use crate::compiler::lsp::compopts::LSPCompilerOpts;
use crate::compiler::lsp::parse::{
    ParsedDoc,
    ParseScope,
    ScopeKind,
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
}

#[derive(Debug)]
pub struct ReparsedHelper {
    pub hash: Vec<u8>,
    pub parsed: HelperForm
}

pub struct ReparsedModule {
    pub args: Rc<SExp>,
    pub helpers: Vec<ReparsedHelper>,
    pub errors: Vec<CompileErr>,
    pub unparsed: HashSet<Vec<u8>>,
    pub exp: Rc<BodyForm>,
    pub includes: HashMap<Vec<u8>, Vec<u8>>
}

pub fn parse_include(
    opts: Rc<dyn CompilerOpts>,
    sexp: Rc<SExp>
) -> Option<Vec<u8>> {
    sexp.proper_list().and_then(|l| {
        if l.len() != 2 {
            return None;
        }

        if let (SExp::Atom(_,incl), SExp::Atom(_,fname)) =
            (l[0].borrow(), l[1].borrow())
        {
            if incl == b"include" {
                return Some(fname.clone());
            }
        }

        None
    })
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
        errors: Vec::new(),
        helpers: Vec::new(),
        includes: HashMap::new(),
        unparsed: HashSet::new(),
        exp: compiled.exp.clone(),
    };

    // if it's a module, we can patch the prefix in, otherwise make a (mod ()
    // prefix for it.
    // We can take the last phrase and if it's not a helper, we can use it as
    // the end of the document.
    let mut took_args = false;

    if simple_ranges.len() == 0 {
        // There's nothing to be gained by trying to do incremental.
        return result;
    } else {
        // Find out if there's a single atom before the first identified
        // expression.
        let docstart = Srcloc::start(&uristring);
        let prefix_start = DocPosition { line: 0, character: 0 };
        let prefix_range = DocRange {
            start: prefix_start.clone(),
            end: simple_ranges[0].start.clone()
        };
        let mut prefix_text = grab_scope_doc_range(doc, &prefix_range, false);
        // TODO hash prefix to prevent reparsing.
        prefix_text.push(b')');
        if let Some(prefix_parse) =
            parse_sexp(docstart, prefix_text.iter().copied()).ok().
            and_then(|s| {
                if s.len() == 0 {
                    None
                } else {
                    s[0].proper_list()
                }
            })
        {
            if prefix_parse.len() > 0 {
                result.args = Rc::new(prefix_parse[prefix_parse.len()-1].clone());
            }
        }

        // Find out of there's a single atom after the last identified atom.
        let suffix_start = simple_ranges[simple_ranges.len()-1].end.clone();
        let suffix_range = DocRange {
            start: suffix_start.clone(),
            end: DocPosition { line: doc.len() as u32, character: 0 }
        };
        let mut suffix_text = grab_scope_doc_range(doc, &suffix_range, false);
        // TODO hash suffix to prevent reparsing.

        let mut break_end = suffix_text.len();

        // Ensure we can parse to the right locations in the source file.
        // Since our parser can handle a list of parsed objects, remove the
        // final paren.

        // Find last )
        for (i, ch) in suffix_text.iter().enumerate() {
            if *ch == b')' {
                break_end = i;
            }
        }

        suffix_text = suffix_text.iter().take(break_end).copied().collect();

        if let Some(suffix_parse) =
            parse_sexp(
                Srcloc::new(
                    Rc::new(uristring.clone()),
                    (suffix_start.line + 1) as usize,
                    (suffix_start.character + 1) as usize
                ),
                suffix_text.iter().copied()
            ).ok()
        {
            if suffix_parse.len() > 0 {
                if let Ok(body) =
                    compile_bodyform(suffix_parse[suffix_parse.len()-1].clone())
                {
                    result.exp = Rc::new(body);
                }
            }
        }
    }

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
                match compile_helperform(opts.clone(), parsed[0].clone()) {
                    Ok(h) => {
                        if let Some(helper) = h.as_ref() {
                            result.helpers.push(ReparsedHelper {
                                hash,
                                parsed: helper.clone()
                            });
                        } else if let Some(include) =
                            parse_include(opts.clone(), parsed[0].clone())
                        {
                            result.includes.insert(hash.clone(), include.clone());
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
                        }
                    },
                    Err(e) => {
                        result.errors.push(e.clone());
                    }
                }
            }
        } else {
            result.unparsed.insert(hash);
        }
    }

    result
}

pub fn combine_new_with_old_parse(
    uristring: &String,
    text: &[Rc<Vec<u8>>],
    parsed: &ParsedDoc,
    reparse: &ReparsedModule
) -> ParsedDoc {
    let mut new_hashes = reparse.unparsed.clone();
    // An exclusive collection from names to hashes.
    // This will let us eliminate functions that have disappeared or renamed.
    let mut new_pointers = HashMap::new();
    let mut new_includes = reparse.includes.clone();

    for new_helper in reparse.helpers.iter() {
        new_hashes.insert(new_helper.hash.clone());
        new_pointers.insert(new_helper.parsed.name().clone(), new_helper.hash.clone());
    }

    let extracted_helpers: Vec<HelperForm> =
        reparse.helpers.iter().map(|h| h.parsed.clone()).collect();

    let mut new_compile =
        parsed.compiled.replace_helpers(&extracted_helpers);

    new_compile.args = reparse.args.clone();
    new_compile.exp = reparse.exp.clone();

    let mut to_remove_helpers = HashSet::new();
    for (k,v) in parsed.name_to_hash.iter() {
        // For any name that isn't already in new_pointers (newly parsed) or
        // that doesn't have a corresponding hash in erparsed.unparsed, we
        // should delete that helper and its pointer.
        if new_pointers.get(k).is_none() {
            if !reparse.unparsed.contains(v) {
                // This is a pointer to something that isn't in the source file
                // anymore.
                to_remove_helpers.insert(k.clone());
            } else {
                new_pointers.insert(k.clone(), v.clone());
            }
        }
    }

    for (h,i) in parsed.includes.iter() {
        // Any hash that's included in neither the old hashes nor the ignored
        // hashes should be discarded.
        if reparse.unparsed.contains(h) {
            new_includes.insert(h.clone(), i.clone());
        }
    }

    ParsedDoc {
        compiled: new_compile.remove_helpers(&to_remove_helpers),
        errors: reparse.errors.clone(),
        scopes: recover_scopes(uristring, &text, &new_compile),
        name_to_hash: new_pointers,
        hashes: new_hashes,
        includes: new_includes
    }
}

impl LSPServiceProvider {
    pub fn with_doc_and_parsed<F,G>(&mut self, uristring: &String, f: F) -> Option<G>
    where
        F: FnOnce(&DocData, &ParsedDoc) -> Option<G>
    {
        self.ensure_parsed_document(uristring);
        if let (Some(d), Some(p)) = (self.get_doc(uristring), self.get_parsed(uristring)) {
            f(&d, &p)
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
            let startloc = Srcloc::start(uristring);
            let output =
                self.parsed_documents.get(uristring).cloned().unwrap_or_else(|| {
                    ParsedDoc::new(startloc)
                });
            let ranges = make_simple_ranges(&doc.text);
            let new_helpers = reparse_subset(
                opts.clone(),
                &doc.text,
                uristring,
                &ranges,
                &output.compiled,
                &output.hashes
            );
            self.save_parse(uristring.clone(), combine_new_with_old_parse(
                uristring, &doc.text, &output, &new_helpers
            ));
        }
    }

    pub fn new() -> Self {
        LSPServiceProvider {
            document_collection: Rc::new(RefCell::new(HashMap::new())),

            parsed_documents: HashMap::new(),
            goto_defs: HashMap::new(),
        }
    }

    pub fn get_file(&self, filename: &String) -> Result<String, String> {
        self.get_doc(filename).map(|d| stringify_doc(&d.text)).unwrap_or_else(|| Err(format!("don't have file {}", filename)))
    }
}
