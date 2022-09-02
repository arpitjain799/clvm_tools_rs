use std::borrow::Borrow;
use std::rc::Rc;

use lsp_types::TextDocumentContentChangeEvent;

use crate::compiler::lsp::LSPServiceProvider;
use crate::compiler::lsp::parse::DocVecByteIter;
use crate::compiler::lsp::types::DocData;

pub trait LSPServiceProviderApplyDocumentPatch {
    fn apply_document_patch(&mut self, uristring: &String, patches: &Vec<TextDocumentContentChangeEvent>);
}

pub fn split_text(td: &String) -> Vec<Rc<Vec<u8>>> {
    let result: Vec<Rc<Vec<u8>>> = td.split("\n").map(|x| Rc::new(x.as_bytes().to_vec())).collect();
    result
}

pub fn stringify_doc(d: &Vec<Rc<Vec<u8>>>) -> Result<String, String> {
    let bytes = DocVecByteIter::new(d).collect();
    String::from_utf8(bytes).map_err(|_| "no conversion from utf8".to_string())
}

impl LSPServiceProviderApplyDocumentPatch for LSPServiceProvider {
    fn apply_document_patch(&mut self, uristring: &String, patches: &Vec<TextDocumentContentChangeEvent>) {
        if let Some(dd) = self.get_doc(uristring) {
            let mut last_line = 1;
            let mut last_col = 1;

            if patches.len() == 1 && patches[0].range.is_none() {
                // We can short circuit a full document rewrite.
                self.save_doc(uristring.clone(), DocData {
                    text: split_text(&patches[0].text)
                });
                return;
            }

            // Try to do an efficient job of patching the old document content.
            let mut doc_copy = dd.text.clone();
            for p in patches.iter() {
                if let Some(r) = p.range {
                    let split_data = split_text(&p.text);
                    let replaced_line = 0;
                    let rstart_line = r.start.line as usize;
                    let rend_line = r.end.line as usize;
                    let rstart_char = r.start.character as usize;
                    let rend_char = r.end.character as usize;
                    let original_end_line = rend_line;
                    for (i, l) in split_data.iter().enumerate() {
                        let match_line = rstart_line + i;
                        let begin_this_line =
                            if rstart_line == i {
                                rstart_char
                            } else {
                                0
                            };
                        let end_this_line =
                            if match_line == rend_line {
                                rend_char
                            } else {
                                doc_copy[match_line].len()
                            };
                        let (prefix, mut suffix) =
                            if match_line == rstart_line {
                                ((&doc_copy[match_line])[0..begin_this_line].to_vec(), Vec::new())
                            } else if match_line == rend_line {
                                (Vec::new(), (&doc_copy[match_line])[end_this_line..].to_vec())
                            } else {
                                (Vec::new(), Vec::new())
                            };
                        let new_line =
                            if prefix.is_empty() && suffix.is_empty() {
                                // Whole line
                                l.clone()
                            } else {
                                // Partial line
                                let mut vec_copy = prefix.clone();
                                let line_borrowed: &Vec<u8> = l.borrow();
                                vec_copy.append(&mut line_borrowed.clone());
                                vec_copy.append(&mut suffix);
                                Rc::new(vec_copy)
                            };

                        if match_line >= original_end_line {
                            // Insert the new line
                            doc_copy.insert(match_line, new_line);
                        } else {
                            // Overwrite line
                            doc_copy[match_line] = new_line;
                        }
                    }
                } else {
                    doc_copy = split_text(&p.text)
                }
            }

            eprintln!("apply document patch: {}", stringify_doc(&doc_copy).unwrap_or_else(|_| "*error*".to_string()));
            self.save_doc(uristring.clone(), DocData {
                text: doc_copy
            });
        }
    }
}
