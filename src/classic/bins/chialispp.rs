use std::env;
use std::fs;
use std::io::Write;

use clvm_tools_rs::compiler::sexp::decode_string;

#[derive(Debug)]
struct Formatter {
    start_paren_level: usize,
    paren_level: usize,
    out_col: usize,
    cur_line: usize,
    line: Vec<u8>,
    lines: Vec<Vec<u8>>,
    def_started: bool,
    last_single_line_comment: usize,
    definition_starts: Vec<usize>,
    extra_def_lines: Vec<usize>,
    result_line: Vec<u8>,
    pub result: Vec<Vec<u8>>,
}

fn trim_ascii_start(line: &Vec<u8>) -> Vec<u8> {
    let mut first_non_ws = 0;
    let mut got_one = false;

    for i in 0..line.len() {
        if !line[i].is_ascii_whitespace() {
            got_one = true;
            first_non_ws = i;
            break;
        }
    }

    if !got_one {
        return Vec::new();
    } else {
        return line[first_non_ws..].to_vec();
    }
}

fn trim_ascii_end(line: &Vec<u8>) -> Vec<u8> {
    let mut last_non_ws = 0;
    let mut got_one = false;

    for i in 0..line.len() {
        if !line[i].is_ascii_whitespace() {
            got_one = true;
            last_non_ws = i;
        }
    }

    if !got_one {
        return Vec::new();
    } else {
        return line[0..last_non_ws+1].to_vec();
    }
}

impl Formatter {
    pub fn new() -> Self {
        Formatter {
            start_paren_level: 0,
            paren_level: 0,
            out_col: 0,
            cur_line: 0,
            line: Vec::new(),
            lines: Vec::new(),
            def_started: false,
            result_line: Vec::new(),
            last_single_line_comment: 0,
            definition_starts: Vec::new(),
            extra_def_lines: Vec::new(),
            result: Vec::new()
        }
    }

    pub fn finish(&mut self) {
        if self.line.len() != 0 {
            self.lines.push(self.line.clone());
            self.line.clear();
        }
        for i in 0..self.lines.len() {
            self.line = self.lines[i].clone();
            self.cur_line = i;
            self.output_line();
        }

        if self.result_line.len() != 0 {
            self.result.push(self.result_line.clone());
        }

        let mut el_idx = 0;
        let mut inserted = 0;

        for ds in self.definition_starts[0..self.definition_starts.len()-1].iter() {
            while el_idx < self.extra_def_lines.len() && self.extra_def_lines[el_idx] < *ds {
                el_idx += 1;
            }

            if el_idx >= self.extra_def_lines.len() {
                break;
            }

            let el = self.extra_def_lines[el_idx];
            if el <= ds + 1 {
                let insert_at = el + inserted;
                self.result.insert(insert_at, Vec::new());
                inserted += 1;
            }
        }
    }

    pub fn finish_line(&mut self) {
        self.lines.push(self.line.clone());
        self.line.clear();
    }

    pub fn output_char(&mut self, ch: u8) {
        if ch == 10 {
            self.result.push(self.result_line.clone());
            self.result_line.clear();
            self.out_col = 0;
        } else {
            self.result_line.push(ch);
            self.out_col += 1;
        }
    }

    pub fn indent(&mut self) {
        while self.out_col < self.start_paren_level * 2 {
            self.output_char(b' ');
        }
    }

    pub fn output_line(&mut self) {
        let mut prec_ws = 0;
        let mut ws_end = 0;
        let mut max_paren_level = self.paren_level;

        self.start_paren_level = self.paren_level;

        if self.line.len() == 0 {
            self.output_char(b'\n');
            return;
        }

        let mut line = trim_ascii_end(&self.line);
        self.line.clear();

        let mut col = 0;
        for i in 0..line.len() {
            let ch = line[i];
            if ch != b' ' && ch != b'\t' {
                prec_ws = col;
                ws_end = i;
                break;
            }

            if ch == b'\t' {
                col = (col & !7) + 8;
            } else {
                col += 1;
            }
        }

        let mut in_string = 0;
        let mut string_bs = false;

        let mut semis = 0;
        let mut semi_loc = 0;
        let mut semi_off = 0;
        let mut comment = Vec::new();

        line = trim_ascii_start(&line);

        for i in 0..line.len() {
            let ch = line[i];

            if self.start_paren_level == 1 && !self.def_started {
                self.def_started = true;
                self.definition_starts.push(self.result.len());
            }

            if string_bs {
                string_bs = false;
                continue;
            }
            if in_string != 0 {
                if ch == b'\\' {
                    string_bs = true;
                }
                if ch == in_string {
                    in_string = 0;
                }
                continue;
            }

            if semis == 0 {
                if ch == b'\'' || ch == b'"' {
                    in_string = ch;
                    continue;
                } else if ch == b'(' {
                    self.paren_level += 1;
                    if self.paren_level > max_paren_level {
                        max_paren_level = self.paren_level;
                    }
                    continue;
                } else if ch == b')' {
                    self.paren_level -= 1;
                    continue;
                }
            }

            if ch == b';' {
                if semis == 0 {
                    semi_loc = i + prec_ws;
                    semi_off = i;
                }
                semis += 1;
            } else if semis > 0 {
                comment = line[i..].to_vec();
                line = trim_ascii_end(&line[0..semi_off].to_vec());
                break;
            }
        }

        // Handle the case where the ;s are the last characters on the line.
        if semis + semi_off == line.len() {
            line = trim_ascii_end(&line[0..semi_off].to_vec());
        }

        line = trim_ascii_end(&line);

        if semis == 1 && self.last_single_line_comment == 0 {
            self.last_single_line_comment = semi_loc;
        } else if semis != 1 {
            self.last_single_line_comment = 0;
        }

        if semis == 1 {
            self.indent();
            for co in line.iter() {
                self.output_char(*co);
            }
            if semi_loc > self.last_single_line_comment {
                self.output_char(b'\n');
            }
            while self.out_col < self.last_single_line_comment {
                self.output_char(b' ');
            }
            self.output_char(b';');
            for co in comment.iter() {
                self.output_char(*co);
            }
        } else if semis > 1 {
            if semis == 2 {
                self.indent();
            }
            for i in 0..semis {
                self.output_char(b';');
            }
            for co in comment.iter() {
                self.output_char(*co);
            }
            if line.len() != 0 {
                // Code after comment in this scenario
                self.output_char(b'\n');
                self.indent();
                for co in line.iter() {
                    self.output_char(*co);
                }
            }
        } else {
            self.indent();
            for co in line.iter() {
                self.output_char(*co);
            }
        }

        self.output_char(b'\n');
        // Insert a line after every definition ends, and record it in extra
        // def lines.
        if max_paren_level > 1 && self.paren_level == 1 {
            // A definition ended.
            self.def_started = false;
            self.extra_def_lines.push(self.result.len());
        }
    }

    pub fn run_char(&mut self, ch: u8) {
        if ch == b'\n' {
            self.finish_line();
        } else {
            self.line.push(ch);
        }
    }
}

fn main() {
    for filename in env::args().skip(1) {
        let filedata = fs::read(filename.clone()).expect(&format!("could not read file {}", filename));
        let mut formatter = Formatter::new();

        for ch in filedata.iter() {
            formatter.run_char(*ch);
        }
        formatter.finish();

        let mut f = fs::File::create(format!("{}.new", filename)).expect(&format!("could not open file {}", filename));
        for line in formatter.result.iter() {
            f.write(line).expect(&format!("could not write file {}", filename));
            f.write("\n".as_bytes());
        }
    }
}
