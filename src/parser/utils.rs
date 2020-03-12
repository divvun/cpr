use std::io::{self, BufRead};

use super::Error;

trait Sink {
    fn push(&mut self, c: u8);
}

impl Sink for Vec<u8> {
    fn push(&mut self, b: u8) {
        Vec::push(self, b);
    }
}

/// Line continuation processor
struct LProcessor<'a> {
    state: LState,
    sink: &'a mut dyn Sink,
}

enum LState {
    Normal,
    Backslash,
}

impl<'a> Sink for LProcessor<'a> {
    fn push(&mut self, b: u8) {
        match self.state {
            LState::Normal => match b {
                b'\\' => {
                    self.state = LState::Backslash;
                }
                b => {
                    self.sink.push(b);
                }
            },
            LState::Backslash => match b {
                b'\r' => {
                    // boo
                }
                b'\n' => {
                    // line continuation
                    self.state = LState::Normal;
                }
                _ => {
                    unimplemented!();
                }
            },
        }
    }
}

/// Comment processor
struct CProcessor<'a> {
    state: CState,
    sink: &'a mut dyn Sink,
}

enum CState {
    Normal,
    NormalSlash,
    /// in single-line comment, waiting for newline
    Sl,
    /// in string literal
    Str,
    /// in string literal, just saw backslash
    StrBackslash,
    /// in multi-line comment
    Ml,
    /// in multi-line comment, just saw star
    MlStar,
}

impl<'a> Sink for CProcessor<'a> {
    fn push(&mut self, b: u8) {
        match self.state {
            CState::Normal => match b {
                b'/' => {
                    self.state = CState::NormalSlash;
                }
                b => {
                    self.sink.push(b);
                }
            },
            CState::NormalSlash => match b {
                b'/' => {
                    self.state = CState::Sl;
                }
                _ => {
                    unimplemented!();
                }
            },
        }
    }
}

pub fn process_line_continuations_and_comments<R: BufRead>(reader: R) -> Result<String, Error> {
    let mut out = Vec::new();
    let mut cproc = CProcessor {
        state: CState::Normal,
        sink: &mut out,
    };
    let mut lcproc = LProcessor {
        state: LState::Normal,
        sink: &mut cproc,
    };

    for b in reader.bytes() {
        lcproc.push(b?);
    }

    let out = String::from_utf8(out)?;
    Ok(out)
}
