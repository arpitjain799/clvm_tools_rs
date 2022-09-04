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

            let mut doc_copy = dd.text.clone();

            // Try to do an efficient job of patching the old document content.
            for p in patches.iter() {
                if let Some(r) = p.range {
                    let split_data = split_text(&p.text);
                    let mut prelude_start =
                        if r.start.line > 0 {
                            dd.text.iter().take((r.start.line - 1) as usize).collect()
                        } else {
                            vec![]
                        };
                    let mut suffix_after =
                        if (r.end.line as usize) < dd.text.len() - 1 {
                            dd.text.iter().skip((r.end.line + 1) as usize).collect()
                        } else {
                            vec![]
                        };
                    let mut line_prefix =
                        if (r.start.line as usize) < dd.text.len() {
                            let line_ref: &Vec<u8> =
                                dd.text[r.start.line as usize].borrow();
                            line_ref.iter().take(r.start.character as usize).copied().collect()
                        } else {
                            vec![]
                        };
                    let mut line_suffix =
                        if (r.end.line as usize) < dd.text.len() {
                            let line_ref: &Vec<u8> =
                                dd.text[r.end.line as usize].borrow();
                            line_ref.iter().skip(r.end.character as usize).copied().collect()
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
                        let mut copied_vec: Vec<u8> = input_line.iter().copied().collect();
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
