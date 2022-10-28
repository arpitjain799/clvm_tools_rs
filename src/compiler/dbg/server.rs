// Based on https://docs.rs/lsp-server/latest/src/lsp_server/lib.rs.html#27-30
// and https://github.com/Chia-Network/vscode-chialisp-lsp/blob/main/runner/src/runner.js
use crate::compiler::dbg::types::MessageHandler;
use crate::compiler::sexp::decode_string;
use serde::{Deserialize, Serialize};
use serde_json;
use std::clone::Clone;
use std::fmt::Debug;

pub struct MessageBuffer<H> {
    eof: bool,
    data: Vec<Vec<u8>>,
    length: Option<usize>,
    pub handler: H,
}

struct MessageByteIter<'a> {
    buffers: &'a [Vec<u8>],
    pub cur: usize,
    pub byt: usize,
}

#[derive(Debug)]
enum ParseState {
    Reading,
    Lf(usize),
    Ended(usize),
}

impl<'a> Iterator for MessageByteIter<'a> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        if self.cur >= self.buffers.len() {
            return None;
        }

        let cur_buf = &self.buffers[self.cur];
        if self.byt >= cur_buf.len() {
            self.cur += 1;
            self.byt = 0;
            return self.next();
        }

        let result = cur_buf[self.byt];
        self.byt += 1;
        Some(result)
    }
}

impl<H> MessageBuffer<H> {
    pub fn new(handler: H) -> MessageBuffer<H> {
        MessageBuffer {
            eof: false,
            data: vec![],
            length: None,
            handler,
        }
    }

    pub fn is_eof(&self) -> bool {
        self.eof
    }

    // Pass whole messages through the message handler and return the reply or
    // error if message decoding failed.
    pub fn receive_bytes<M>(&mut self, buf: &[u8]) -> Result<Option<Vec<u8>>, String>
    where
        H: MessageHandler<M>,
        for<'a> M: Serialize + Deserialize<'a> + Debug + Clone,
    {
        eprintln!("receive {:?}", buf);
        if self.eof {
            return Ok(None);
        }

        if buf.is_empty() {
            return Ok(Some(vec![]));
        }

        let mut out_messages = Vec::new();
        self.data.push(buf.to_owned());

        while !self.eof {
            if let Some(msgdata) = self.extract_message()? {
                let new_messages: Option<Vec<M>> = self.process_message(&msgdata)?;
                match &new_messages {
                    Some(msgs) => {
                        for m in msgs.iter().cloned() {
                            let json_msg = serde_json::to_value(&m).map_err(|_| {
                                format!("could not convert outbound message {:?} to json", m)
                            })?;
                            let encoded_msg = json_msg.to_string().as_bytes().to_vec();
                            out_messages.push(encoded_msg);
                        }
                    }
                    _ => {
                        self.eof = true;
                    }
                }
            } else {
                break;
            }
        }

        let mut result_bytes = Vec::new();
        if out_messages.is_empty() {
            return Ok(None);
        }

        for encoded_msg in out_messages.iter() {
            let msg_len = encoded_msg.len();
            result_bytes.append(
                &mut format!("Content-Length: {}\r\n\r\n", msg_len)
                    .as_bytes()
                    .to_vec(),
            );
            result_bytes.append(&mut encoded_msg.to_vec());
        }

        Ok(Some(result_bytes))
    }

    pub fn process_message<M>(&mut self, msgdata: &[u8]) -> Result<Option<Vec<M>>, String>
    where
        H: MessageHandler<M>,
        for<'a> M: Serialize + Deserialize<'a> + Debug + Clone,
    {
        let msg_string = decode_string(&msgdata);
        let msg: M = serde_json::from_str(&msg_string)
            .map_err(|_| format!("failed to decode {}", msg_string))?;
        self.handler.handle_message(&msg)
    }

    fn data_iter(&self) -> MessageByteIter {
        MessageByteIter {
            buffers: &self.data,
            cur: 0,
            byt: 0,
        }
    }

    fn len(&self) -> usize {
        let mut total = 0;
        for b in self.data.iter() {
            total += b.len();
        }
        total
    }

    fn chop_data(&mut self, chop_prefix_bytes: usize) {
        let cur_len = self.len();
        if cur_len <= chop_prefix_bytes {
            self.data = vec![];
        } else {
            let new_data = self.data_iter().skip(chop_prefix_bytes).collect();
            self.data = vec![new_data];
        }
    }

    fn extract_message(&mut self) -> Result<Option<Vec<u8>>, String> {
        if let Some(l) = self.length {
            let have_data = self.len();
            if have_data < l {
                return Ok(None);
            }

            self.length = None;
            let message_data: Vec<u8> = self.data_iter().take(l).collect();
            self.chop_data(l);
            Ok(Some(message_data))
        } else {
            // Try to recognize a set of headers containing content-length,
            let mut s = ParseState::Reading;
            let mut read_line = Vec::new();
            let mut header_lines = Vec::new();

            for (i, by) in self.data_iter().enumerate() {
                match (&s, by) {
                    (ParseState::Reading, b'\r') => {
                        if !read_line.is_empty() {
                            header_lines.push(read_line);
                            read_line = vec![];
                        }
                        s = ParseState::Lf(0);
                    }
                    (ParseState::Reading, b'\n') => {
                        if !read_line.is_empty() {
                            header_lines.push(read_line);
                            read_line = vec![];
                        }
                        s = ParseState::Lf(1);
                    }
                    (ParseState::Reading, b) => {
                        read_line.push(b);
                    }
                    (ParseState::Lf(n), b'\r') => {
                        s = ParseState::Lf(*n);
                    }
                    (ParseState::Lf(n), b'\n') => {
                        if *n == 1 {
                            s = ParseState::Ended(i + 1);
                            break;
                        } else {
                            s = ParseState::Lf(n + 1);
                        }
                    }
                    (ParseState::Lf(_), b) => {
                        s = ParseState::Reading;
                        read_line.push(b);
                    }
                    _ => {
                        return Err("No way to match past ending of headers".to_string());
                    }
                }
            }

            // Found a \r\n\r\n, deecode headers.
            if let ParseState::Ended(header_len) = s {
                for line in header_lines.iter().take(header_len) {
                    let found_colon = line.iter().position(|b| *b == b':');
                    if let Some(c) = found_colon {
                        let before_colon_bytes: Vec<u8> = line.iter().take(c).copied().collect();
                        let string_before_colon = decode_string(&before_colon_bytes);
                        let after_colon_bytes: Vec<u8> = line.iter().skip(c + 1).copied().collect();
                        let string_after_colon = decode_string(&after_colon_bytes);
                        if string_before_colon.trim().to_lowercase() == "content-length" {
                            let found_length = string_after_colon
                                .trim()
                                .to_string()
                                .parse::<usize>()
                                .map_err(|_| {
                                    format!(
                                        "could not interpret content length {}",
                                        string_after_colon
                                    )
                                })?;
                            self.length = Some(found_length);
                            break;
                        }
                    } else {
                        return Err(format!("header needs colon: {}", decode_string(&line)));
                    }
                }

                if let Some(_) = self.length {
                    self.chop_data(header_len);
                    self.extract_message()
                } else {
                    Err("scanned headers but didn't find content-length".to_string())
                }
            } else {
                // Not a full header set yet.
                Ok(None)
            }
        }
    }
}
