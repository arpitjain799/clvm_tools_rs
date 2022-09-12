use std::borrow::Borrow;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use crate::compiler::clvm::sha256tree_from_atom;
use crate::compiler::comptypes::{
    BodyForm,
    CompileErr,
    CompileForm,
    CompilerOpts,
    HelperForm
};
use crate::compiler::frontend::{
    compile_bodyform,
    compile_helperform
};
use crate::compiler::sexp::{
    SExp,
    decode_string,
    parse_sexp
};
use crate::compiler::srcloc::Srcloc;
use crate::compiler::lsp::parse::{
    ParsedDoc,
    grab_scope_doc_range,
    recover_scopes
};
use crate::compiler::lsp::types::{
    DocPosition,
    DocRange
};

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

