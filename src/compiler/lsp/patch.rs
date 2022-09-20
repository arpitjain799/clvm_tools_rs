use std::borrow::Borrow;
use std::collections::HashMap;
use std::rc::Rc;

use lsp_types::TextDocumentContentChangeEvent;

use crate::compiler::lsp::parse::DocVecByteIter;
use crate::compiler::lsp::types::DocData;
use crate::compiler::lsp::LSPServiceProvider;

pub trait PatchableDocument {
    fn apply_patch(&self, version: i32, patches: &[TextDocumentContentChangeEvent]) -> Self;
}

pub trait LSPServiceProviderApplyDocumentPatch {
    fn apply_document_patch(
        &mut self,
        uristring: &str,
        version: i32,
        patches: &[TextDocumentContentChangeEvent],
    );
}

pub fn split_text(td: &str) -> Vec<Rc<Vec<u8>>> {
    let result: Vec<Rc<Vec<u8>>> = td
        .split('\n')
        .map(|x| Rc::new(x.as_bytes().to_vec()))
        .collect();
    result
}

pub fn stringify_doc(d: &[Rc<Vec<u8>>]) -> Result<String, String> {
    let bytes = DocVecByteIter::new(d).collect();
    String::from_utf8(bytes).map_err(|_| "no conversion from utf8".to_string())
}

pub fn redo_comment_line(map: &mut HashMap<usize, usize>, text: &[Rc<Vec<u8>>], line: usize) {
    let empty_line = Vec::new();
    let text_b: &Vec<u8> = if line >= text.len() {
        &empty_line
    } else {
        text[line].borrow()
    };
    if let Some(found) = text_b.iter().position(|ch| *ch == b';') {
        map.insert(line, found);
    } else {
        map.remove(&line);
    }
}

pub fn compute_comment_lines(text: &[Rc<Vec<u8>>]) -> HashMap<usize, usize> {
    let mut res = HashMap::new();
    for i in 0..text.len() {
        redo_comment_line(&mut res, text, i);
    }
    res
}

impl PatchableDocument for DocData {
    fn apply_patch(&self, version: i32, patches: &[TextDocumentContentChangeEvent]) -> Self {
        let mut doc_copy = self.text.clone();
        let mut comments_copy = self.comments.clone();

        // Try to do an efficient job of patching the old document content.
        for p in patches.iter() {
            let old_lines = doc_copy.len();

            if let Some(r) = p.range {
                let prelude_start = if r.start.line > 0 {
                    self.text.iter().take(r.start.line as usize).collect()
                } else {
                    vec![]
                };
                let suffix_after = if (r.end.line as usize) < self.text.len() - 1 {
                    self.text.iter().skip((r.end.line + 1) as usize).collect()
                } else {
                    vec![]
                };
                let mut line_prefix = if (r.start.line as usize) < self.text.len() {
                    let line_ref: &Vec<u8> = self.text[r.start.line as usize].borrow();
                    line_ref
                        .iter()
                        .take(r.start.character as usize)
                        .copied()
                        .collect()
                } else {
                    vec![]
                };
                let mut line_suffix = if (r.end.line as usize) < self.text.len() {
                    let line_ref: &Vec<u8> = self.text[r.end.line as usize].borrow();
                    line_ref
                        .iter()
                        .skip(r.end.character as usize)
                        .copied()
                        .collect()
                } else {
                    vec![]
                };

                let split_input = split_text(&p.text);
                // Assemble the result:
                // prelude_start lines
                // line_prelude + split_input[0]
                // split_input[1..len - 2]
                // split_input[len - 1] + line_suffix
                // suffix_after

                doc_copy.clear();
                for line in prelude_start.iter() {
                    let line_borrow: &Vec<u8> = (*line).borrow();
                    doc_copy.push(Rc::new(line_borrow.clone()));
                }

                if split_input.is_empty() {
                    line_prefix.append(&mut line_suffix);
                } else if split_input.len() == 1 {
                    let input_line: &Vec<u8> = split_input[0].borrow();
                    let mut copied_vec: Vec<u8> = input_line.to_vec();
                    line_prefix.append(&mut copied_vec);
                    line_prefix.append(&mut line_suffix);
                    doc_copy.push(Rc::new(line_prefix));
                } else {
                    let first_input_line: &Vec<u8> = split_input[0].borrow();
                    line_prefix.append(&mut first_input_line.clone());
                    doc_copy.push(Rc::new(line_prefix));
                    for in_line in split_input.iter().skip(1).take(split_input.len() - 2) {
                        let input_line: &Vec<u8> = in_line.borrow();
                        doc_copy.push(Rc::new(input_line.clone()));
                    }
                    let last_input_line: &Vec<u8> = split_input[split_input.len() - 1].borrow();
                    let mut last_input = last_input_line.clone();
                    last_input.append(&mut line_suffix);
                    doc_copy.push(Rc::new(last_input));
                }

                for line in suffix_after.iter() {
                    let line_borrow: &Vec<u8> = (*line).borrow();
                    doc_copy.push(Rc::new(line_borrow.clone()));
                }

                let new_lines = doc_copy.len();
                if old_lines > new_lines {
                    for i in new_lines..old_lines {
                        comments_copy.remove(&i);
                    }
                }
                for i in (r.start.line as usize)..(r.start.line as usize) + split_input.len() + 1 {
                    redo_comment_line(&mut comments_copy, &doc_copy, i);
                }
            } else {
                doc_copy = split_text(&p.text);
                comments_copy = compute_comment_lines(&doc_copy);
            }
        }

        DocData {
            text: doc_copy,
            comments: comments_copy,
            version,
        }
    }
}

impl LSPServiceProviderApplyDocumentPatch for LSPServiceProvider {
    fn apply_document_patch(
        &mut self,
        uristring: &str,
        version: i32,
        patches: &[TextDocumentContentChangeEvent],
    ) {
        if let Some(dd) = self.get_doc(uristring) {
            if patches.len() == 1 && patches[0].range.is_none() {
                // We can short circuit a full document rewrite.
                // There are no hanging patches as a result.
                let have_text = split_text(&patches[0].text);
                let comments = compute_comment_lines(&have_text);
                self.save_doc(
                    uristring.to_owned(),
                    DocData {
                        text: have_text,
                        version,
                        comments,
                    },
                );
                return;
            }

            let new_doc = dd.apply_patch(version, patches);
            self.save_doc(uristring.to_owned(), new_doc);
        }
    }
}
