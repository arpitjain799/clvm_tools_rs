use std::borrow::Borrow;
use std::cell::{Ref, RefCell};
use std::cmp::{Ordering, PartialOrd};
use std::collections::{BTreeMap, HashMap};
use std::mem::swap;
use std::rc::Rc;

use lsp_server::{
    ExtractError,
    Notification,
    Message,
    Request,
    RequestId
};

use lsp_types::{
    Diagnostic,
    Position,
    PublishDiagnosticsParams,
    Range,
    SemanticTokenModifier,
    SemanticTokenType,
    Url,

    OneOf,

    CompletionOptions,
    SemanticTokensLegend,
    SemanticTokensFullOptions,
    SemanticTokensOptions,
    SemanticTokensServerCapabilities,
    ServerCapabilities,
    TextDocumentSyncCapability,
    TextDocumentSyncKind,
    WorkDoneProgressOptions,
};

use crate::compiler::srcloc::Srcloc;
use crate::compiler::lsp::compopts::LSPCompilerOpts;
use crate::compiler::lsp::parse::{
    ParsedDoc,
    make_simple_ranges
};
use crate::compiler::lsp::patch::{
    stringify_doc
};
use crate::compiler::lsp::reparse::{
    combine_new_with_old_parse,
    reparse_subset
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
    pub loc: DocRange
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
    // Waiting for init msg.
    pub waiting_for_init: bool,

    // Let document collection be sharable due to the need to capture it for
    // use in compiler opts.
    pub document_collection: Rc<RefCell<HashMap<String, DocData>>>,

    // These aren't shared.
    pub parsed_documents: HashMap<String, ParsedDoc>,
    pub goto_defs: HashMap<String, BTreeMap<SemanticTokenSortable, Srcloc>>,
}

impl LSPServiceProvider {
    pub fn parse_document_and_output_errors(&mut self, uristring: &str) -> Vec<Message> {
        let mut res = Vec::new();

        self.ensure_parsed_document(uristring);
        if let Some(p) = self.get_parsed(uristring) {
            let mut errors = Vec::new();

            for (_, error) in p.errors.iter() {
                eprintln!("error {:?}", error);
                errors.push(Diagnostic {
                    range: DocRange::from_srcloc(error.0.clone()).to_range(),
                    severity: None,
                    code: None,
                    code_description: None,
                    source: Some("chialisp".to_string()),
                    message: error.1.clone(),
                    tags: None,
                    related_information: None,
                    data: None
                });
            }

            if !errors.is_empty() {
                res.push(Message::Notification(Notification {
                    method: "textDocument/publishDiagnostics".to_string(),
                    params: serde_json::to_value(PublishDiagnosticsParams {
                        uri: Url::parse(uristring).unwrap(),
                        version: None,
                        diagnostics: errors
                    }).unwrap()
                }));
            }
        }

        res
    }

    pub fn with_doc_and_parsed<F,G>(&mut self, uristring: &str, f: F) -> Option<G>
    where
        F: FnOnce(&DocData, &ParsedDoc) -> Option<G>
    {
        if let (Some(d), Some(p)) = (self.get_doc(uristring), self.get_parsed(uristring)) {
            f(&d, &p)
        } else {
            None
        }
    }

    pub fn get_doc(&self, uristring: &str) -> Option<DocData> {
        let cell: &RefCell<HashMap<String, DocData>> = self.document_collection.borrow();
        let coll: Ref<HashMap<String, DocData>> = cell.borrow();
        (&coll).get(uristring).cloned()
    }

    pub fn get_parsed(&self, uristring: &str) -> Option<ParsedDoc> {
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

    pub fn ensure_parsed_document(
        &mut self,
        uristring: &str
    ) {
        let opts = Rc::new(LSPCompilerOpts::new(uristring, self.document_collection.clone()));

        if let Some(doc) = self.get_doc(uristring) {
            let startloc = Srcloc::start(uristring);
            let output =
                self.parsed_documents.get(uristring).cloned().unwrap_or_else(|| {
                    ParsedDoc::new(startloc)
                });
            let ranges = make_simple_ranges(&doc.text);
            let new_helpers = reparse_subset(
                opts,
                &doc.text,
                uristring,
                &ranges,
                &output.compiled,
                &output.hashes
            );
            self.save_parse(uristring.to_owned(), combine_new_with_old_parse(
                uristring, &doc.text, &output, &new_helpers
            ));
        }
    }

    pub fn get_capabilities() -> ServerCapabilities {
        ServerCapabilities {
            // Specify capabilities from the set:
            // https://docs.rs/lsp-types/latest/lsp_types/struct.ServerCapabilities.html
            definition_provider: Some(OneOf::Left(true)),
            semantic_tokens_provider: Some(SemanticTokensServerCapabilities::SemanticTokensOptions(SemanticTokensOptions {
                work_done_progress_options: WorkDoneProgressOptions {
                    work_done_progress: Some(false)
                },
                legend: SemanticTokensLegend {
                    token_types: TOKEN_TYPES.clone(),
                    token_modifiers: TOKEN_MODIFIERS.clone(),
                },
                range: None,
                full: Some(SemanticTokensFullOptions::Delta {delta: Some(true)})
            })),
            text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::INCREMENTAL)),
            completion_provider: Some(CompletionOptions {
                resolve_provider: Some(true),
                //             trigger_characters: Some(completion_start),
                ..Default::default()
            }),
            ..Default::default()
        }
    }

    pub fn new(configured: bool) -> Self {
        LSPServiceProvider {
            waiting_for_init: !configured,
            document_collection: Rc::new(RefCell::new(HashMap::new())),

            parsed_documents: HashMap::new(),
            goto_defs: HashMap::new(),
        }
    }

    pub fn get_file(&self, filename: &str) -> Result<String, String> {
        self.get_doc(filename).map(|d| stringify_doc(&d.text)).unwrap_or_else(|| Err(format!("don't have file {}", filename)))
    }
}
