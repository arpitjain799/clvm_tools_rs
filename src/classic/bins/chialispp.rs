use std::env;
use std::io;
use std::fs;

use clvm_tools_rs::compiler::sexp::decode_string;

#[derive(Debug)]
struct Formatter {
    start_paren_level: usize,
    paren_level: usize,
    out_col: usize,
    line: Vec<u8>,
    last_single_line_comment: usize,
    pub result: Vec<u8>,
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
            line: Vec::new(),
            last_single_line_comment: 0,
            result: Vec::new()
        }
    }

    pub fn finish(&mut self) {
        if self.line.len() != 0 {
            self.output_line();
        }
    }

    pub fn output_char(&mut self, ch: u8) {
        self.result.push(ch);
        if ch == 10 {
            self.out_col = 0;
        } else {
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
    }

    pub fn run_char(&mut self, ch: u8) {
        if ch == b'\n' {
            self.output_line();
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

        let out_str = decode_string(&formatter.result);
        fs::write(format!("{}.new", filename), out_str).expect(&format!("could not write file {}", filename));
    }
}
