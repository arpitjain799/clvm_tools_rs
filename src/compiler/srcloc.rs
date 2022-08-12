use std::rc::Rc;
use std::borrow::Borrow;

#[derive(Clone, Debug, PartialEq)]
pub struct Until {
    pub line: usize,
    pub col: usize
}

impl Until {
    pub fn from_pair(p : (usize, usize)) -> Self {
        Until { line: p.0, col: p.1 }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Srcloc {
    pub file: Rc<String>,
    pub line: usize,
    pub col: usize,
    pub until: Option<Until>,
}

// let srcLocationToJson sl =
//   let b =
//     [ ("line", Js.Json.number (float_of_int sl.line))
//     ; ("col", Js.Json.number (float_of_int sl.col))
//     ]
//   in
//   let u =
//     match sl.until with
//     | None -> []
//     | Some (l,c) ->
//       [ ("ml", Js.Json.number (float_of_int l))
//       ; ("mc", Js.Json.number (float_of_int c))
//       ]
//   in
//   List.concat [ b ; u ]
//   |> Js.Dict.fromList
//   |> Js.Json.object_

impl Srcloc {
    pub fn to_string(&self) -> String {
        match &self.until {
            None => format!("{}({}):{}", self.file, self.line, self.col),
            Some(u) => format!(
                "{}({}):{}-{}({}):{}",
                self.file, self.line, self.col, self.file, u.line, u.col
            ),
        }
    }

    pub fn new(name: Rc<String>, line: usize, col: usize) -> Self {
        Srcloc {
            file: name,
            line: line,
            col: col,
            until: None
        }
    }

    pub fn overlap(&self, other: &Srcloc) -> bool {
        let mf: &String = self.file.borrow();
        let of: &String = other.file.borrow();
        if mf != of {
            return false;
        }

        if self.line == other.line && self.col == other.col {
            return true;
        }

        match (self.until.as_ref(), other.until.as_ref()) {
            (None, None) => self.line == other.line && self.col == other.col,
            (None, Some(_)) => other.overlap(self),
            (Some(u), None) => {
                if self.line < other.line && u.line > other.line {
                    return true;
                }
                if self.line == other.line && self.col <= other.col && self.col + self.len() >= other.col {
                    return true;
                }
                if u.line == other.line && self.col + self.len() >= other.col {
                    return true;
                }

                false
            },
            (Some(u1), Some(u2)) => {
                let l1 = Srcloc::new(self.file.clone(), self.line, self.col);
                let l2 = Srcloc::new(self.file.clone(), u1.line, u1.col);
                let l3 = Srcloc::new(self.file.clone(), other.line, other.col);
                let l4 = Srcloc::new(self.file.clone(), u2.line, u2.col);
                other.overlap(&l1) || other.overlap(&l2) || self.overlap(&l3) || self.overlap(&l4)
            }
        }
    }

    pub fn len(&self) -> usize {
        if let Some(u) = &self.until {
            if u.line != self.line {
                1 // TODO: Can't tell length ...
                // We can fix this by recording the character
                // number in the file.
            } else {
                u.col - self.col
            }
        } else {
            1
        }
    }

    pub fn ext(&self, other: &Srcloc) -> Srcloc {
        if other.file == self.file {
            combine_src_location(self, other)
        } else {
            self.clone()
        }
    }

    pub fn advance(&self, ch: u8) -> Srcloc {
        match ch as char {
            '\n' => Srcloc {
                file: self.file.clone(),
                col: 1,
                line: self.line + 1,
                until: self.until.clone(),
            },
            '\t' => {
                let next_tab = (self.col + 8) & !7;
                Srcloc {
                    file: self.file.clone(),
                    col: next_tab,
                    line: self.line,
                    until: self.until.clone(),
                }
            }
            _ => Srcloc {
                file: self.file.clone(),
                col: self.col + 1,
                line: self.line,
                until: self.until.clone(),
            },
        }
    }

    pub fn start(file: &String) -> Srcloc {
        Srcloc {
            file: Rc::new(file.to_string()),
            line: 1,
            col: 1,
            until: None,
        }
    }
}

pub fn src_location_min(a: &Srcloc) -> (usize, usize) {
    (a.line, a.col)
}

pub fn src_location_max(a: &Srcloc) -> (usize, usize) {
    match &a.until {
        None => (a.line, a.col + 1),
        Some(u) => (u.line, u.col),
    }
}

fn add_onto(x: &Srcloc, y: &Srcloc) -> Srcloc {
    Srcloc {
        file: x.file.clone(),
        line: x.line,
        col: x.col,
        until: Some(Until::from_pair(src_location_max(y))),
    }
}

pub fn combine_src_location(a: &Srcloc, b: &Srcloc) -> Srcloc {
    if a.line < b.line {
        add_onto(a, b)
    } else if a.line == b.line {
        if a.col < b.col {
            add_onto(a, b)
        } else if a.col == b.col {
            a.clone()
        } else {
            add_onto(b, a)
        }
    } else {
        add_onto(b, a)
    }
}
