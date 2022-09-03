use std::borrow::Borrow;
use std::cell::{Ref, RefCell};
use std::collections::{BTreeMap, HashMap};
use std::mem::swap;
use std::rc::Rc;

use lsp_server::{
    ExtractError,
    Request,
    RequestId
};

use lsp_types::{
    Position,
    SemanticTokenModifier,
    SemanticTokenType
};

use crate::compiler::lsp::compopts::LSPCompilerOpts;
use crate::compiler::lsp::parse::{
    ParsedDoc,
    ParseOutput,
    ParseResult
};
use crate::compiler::lsp::patch::stringify_doc;
use crate::compiler::lsp::semtok::SemanticTokenSortable;
use crate::compiler::srcloc::Srcloc;

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

pub struct LSPServiceProvider {
    pub document_collection: Rc<RefCell<HashMap<String, DocData>>>,
    pub parsed_documents: Rc<RefCell<HashMap<String, ParsedDoc>>>,
    pub goto_defs: HashMap<String, BTreeMap<SemanticTokenSortable, Srcloc>>,
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
        let cell: &RefCell<HashMap<String, ParsedDoc>> = self.parsed_documents.borrow();
        let coll: Ref<HashMap<String, ParsedDoc>> = cell.borrow();
        coll.get(uristring).cloned()
    }

    pub fn save_doc(&self, uristring: String, dd: DocData) {
        let cell: &RefCell<HashMap<String, DocData>> = self.document_collection.borrow();
        cell.replace_with(|coll| {
            let mut repl = HashMap::new();
            swap(&mut repl, coll);
            repl.insert(uristring.clone(), dd);
            repl
        });
        let pcell: &RefCell<HashMap<String, ParsedDoc>> = self.parsed_documents.borrow();
        pcell.replace_with(|pcoll| {
            let mut repl = HashMap::new();
            swap(&mut repl, pcoll);
            repl.remove(&uristring);
            repl
        });
    }

    fn save_parse(&self, uristring: String, p: ParsedDoc) {
        let cell: &RefCell<HashMap<String, ParsedDoc>> = self.parsed_documents.borrow();
        cell.replace_with(|pcoll| {
            let mut repl = HashMap::new();
            swap(&mut repl, pcoll);
            repl.insert(uristring, p);
            repl
        });
    }

    pub fn ensure_parsed_document<'a>(
        &mut self,
        uristring: &String
    ) {
        if let Some(doc) = self.get_doc(uristring) {
            let opts = Rc::new(LSPCompilerOpts::new(uristring.clone(), self.document_collection.clone()));
            let parsed = ParsedDoc::new(opts, uristring, &doc.text);
            self.save_parse(uristring.clone(), parsed);
        }
    }

    pub fn new() -> Self {
        LSPServiceProvider {
            document_collection: Rc::new(RefCell::new(HashMap::new())),
            parsed_documents: Rc::new(RefCell::new(HashMap::new())),
            goto_defs: HashMap::new()
        }
    }

    pub fn get_file(&self, filename: &String) -> Result<String, String> {
        self.get_doc(filename).map(|d| stringify_doc(&d.text)).unwrap_or_else(|| Err(format!("don't have file {}", filename)))
    }
}
