use std::env;
use std::fs;
use std::io::Write;

#[derive(Debug)]
struct Formatter {
    start_paren_level: usize,
    paren_level: usize,
    out_col: usize,
    cur_line: usize,
    line: Vec<u8>,
    lines: Vec<Vec<u8>>,
    def_started: bool,
    getting_form_name: u8,
    form_name: Vec<u8>,
    got_form_on_line: usize,
    reset_form_indent: bool,
    last_single_line_comment: usize,
    definition_starts: Vec<usize>,
    extra_def_lines: Vec<usize>,
    result_line: Vec<u8>,
    indent_stack: Vec<usize>,
    pub result: Vec<Vec<u8>>,
}

fn trim_ascii_start(line: &[u8]) -> Vec<u8> {
    let mut first_non_ws = 0;
    let mut got_one = false;

    for (i, ch) in line.iter().enumerate() {
        if !ch.is_ascii_whitespace() {
            got_one = true;
            first_non_ws = i;
            break;
        }
    }

    if !got_one {
        Vec::new()
    } else {
        line[first_non_ws..].to_vec()
    }
}

fn trim_ascii_end(line: &[u8]) -> Vec<u8> {
    let mut last_non_ws = 0;
    let mut got_one = false;

    for (i, ch) in line.iter().enumerate() {
        if !ch.is_ascii_whitespace() {
            got_one = true;
            last_non_ws = i;
        }
    }

    if !got_one {
        Vec::new()
    } else {
        line[0..last_non_ws+1].to_vec()
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
            getting_form_name: 0,
            got_form_on_line: 0,
            form_name: Vec::new(),
            reset_form_indent: false,
            def_started: false,
            result_line: Vec::new(),
            last_single_line_comment: 0,
            definition_starts: Vec::new(),
            extra_def_lines: Vec::new(),
            indent_stack: Vec::new(),
            result: Vec::new()
        }
    }

    pub fn finish(&mut self) {
        if !self.line.is_empty() {
            self.lines.push(self.line.clone());
            self.line.clear();
        }
        for i in 0..self.lines.len() {
            self.line = self.lines[i].clone();
            self.cur_line = i;
            self.output_line();
        }

        if !self.result_line.is_empty() {
            self.result.push(self.result_line.clone());
        }

        let mut el_idx = 0;
        let mut inserted = 0;

        for ds in self.definition_starts[0..self.definition_starts.len()].iter() {
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

    pub fn indent(&mut self, cur_indent: usize) {
        while self.out_col < cur_indent {
            self.output_char(b' ');
        }
    }

    pub fn get_cur_indent(&mut self) -> usize {
        if !self.indent_stack.is_empty() {
            self.indent_stack[self.indent_stack.len()-1]
        } else {
            0
        }
    }

    pub fn reset_indent(&mut self, i: usize) {
        if !self.indent_stack.is_empty() {
            let idx = self.indent_stack.len()-1;
            self.indent_stack[idx] = i;
        }
    }

    pub fn indent_paren(&mut self) {
        let current_indent =
            if !self.indent_stack.is_empty() {
                self.indent_stack[self.indent_stack.len() - 1]
            } else {
                0
            };
        self.indent_stack.push(current_indent + 2);
    }

    pub fn retire_indent(&mut self) {
        if !self.indent_stack.is_empty() {
            self.indent_stack.remove(self.indent_stack.len()-1);
        }
    }

    pub fn output_line(&mut self) {
        let line_indent = self.get_cur_indent();
        let mut prec_ws = 0;
        let mut max_paren_level = self.paren_level;

        self.start_paren_level = self.paren_level;

        if self.line.is_empty() {
            self.output_char(b'\n');
            return;
        }

        let mut line = trim_ascii_end(&self.line);
        self.line.clear();

        let mut col = 0;
        for ch in line.iter() {
            if *ch != b' ' && *ch != b'\t' {
                prec_ws = col;
                break;
            }

            if *ch == b'\t' {
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

            if self.getting_form_name > 0 {
                self.reset_form_indent = false;
                if self.getting_form_name == 1 && !ch.is_ascii_whitespace() {
                    self.getting_form_name = 2;
                    self.form_name.push(ch);
                } else if self.getting_form_name == 2 && ch.is_ascii_whitespace() || ch == b'(' || ch == b')' {
                    self.getting_form_name = 0;
                    self.got_form_on_line = self.cur_line;
                } else {
                    self.form_name.push(ch);
                }
            }

            if self.start_paren_level == 1 && !self.def_started {
                self.def_started = true;
                self.definition_starts.push(self.result.len());
            }

            let should_reset_indent = self.getting_form_name == 0 && self.form_name == "if".as_bytes().to_vec() && !ch.is_ascii_whitespace() && !self.reset_form_indent;
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
                    // If the previous level was an 'if', reset the indent.
                    if self.paren_level > max_paren_level {
                        max_paren_level = self.paren_level;
                    }

                    self.paren_level += 1;
                    if should_reset_indent {
                        self.reset_indent(line_indent + i);
                        self.reset_form_indent = true;
                    }
                    self.indent_paren();

                    self.form_name.clear();
                    self.got_form_on_line = 0;
                    self.getting_form_name = 1;
                    continue;
                } else if ch == b')' {
                    self.paren_level -= 1;
                    self.retire_indent();
                    continue;
                } else if should_reset_indent {
                    self.reset_form_indent = true;
                    self.reset_indent(line_indent + i);
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

        match (semis, semis > 1) {
            (1, _) => {
                self.indent(line_indent);
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
            }
            (_, true) => {
                if semis == 2 {
                    self.indent(line_indent);
                }
                for _i in 0..semis {
                    self.output_char(b';');
                }
                for co in comment.iter() {
                    self.output_char(*co);
                }
                if !line.is_empty() {
                    // Code after comment in this scenario
                    self.output_char(b'\n');
                    self.indent(line_indent);
                    for co in line.iter() {
                        self.output_char(*co);
                    }
                }
            }
            _ => {
                self.indent(line_indent);
                for co in line.iter() {
                    self.output_char(*co);
                }
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
        let filedata = fs::read(filename.clone()).unwrap_or_else(|_| panic!("could not read file {}", filename));
        let mut formatter = Formatter::new();

        for ch in filedata.iter() {
            formatter.run_char(*ch);
        }
        formatter.finish();

        let mut f = fs::File::create(format!("{}.new", filename)).unwrap_or_else(|_| panic!("could not open file {}", filename));
        for line in formatter.result.iter() {
            f.write_all(line).unwrap_or_else(|_| panic!("could not write file {}", filename));
            f.write_all("\n".as_bytes()).unwrap_or_else(|_| panic!("could not write file {}", filename));
        }
    }
}
