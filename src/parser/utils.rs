use std::io::BufRead;

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
    fn push(&mut self, v: u8) {
        match self.state {
            LState::Normal => match v {
                b'\\' => {
                    self.state = LState::Backslash;
                }
                v => {
                    self.sink.push(v);
                }
            },
            LState::Backslash => match v {
                b'\r' => {
                    // ah, CRLF. Ignore and stay in line continuation state
                }
                b'\n' => {
                    // line continuation
                    self.state = LState::Normal;
                }
                v => {
                    // not a line continuation, nevermind
                    self.sink.push(b'\\');
                    self.sink.push(v);
                    self.state = LState::Normal;
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
    Single,
    /// in string literal
    String,
    /// in string literal, just saw backslash
    StringBackslash,
    /// in multi-line comment
    Multi,
    /// in multi-line comment, just saw star
    MultiStar,
}

impl<'a> Sink for CProcessor<'a> {
    fn push(&mut self, v: u8) {
        match self.state {
            CState::Normal => match v {
                b'/' => {
                    self.state = CState::NormalSlash;
                }
                b'"' => {
                    self.sink.push(b'"');
                    self.state = CState::String;
                }
                v => {
                    self.sink.push(v);
                }
            },
            CState::String => match v {
                b'"' => {
                    // end of string
                    self.sink.push(b'"');
                    self.state = CState::Normal;
                }
                b'\\' => {
                    // start of escape sequence
                    self.state = CState::StringBackslash;
                }
                v => {
                    self.sink.push(v);
                }
            },
            CState::StringBackslash => match v {
                v => {
                    // this state exists for `\"` pretty much
                    self.sink.push(b'\\');
                    self.sink.push(v);
                    self.state = CState::String;
                }
            },
            CState::NormalSlash => match v {
                b'/' => {
                    // start of single-line comment
                    self.state = CState::Single;
                }
                b'*' => {
                    // start of multi-line comment
                    self.state = CState::Multi;
                }
                v => {
                    // just a good old slash
                    self.sink.push(b'/');
                    self.sink.push(v);
                    self.state = CState::Normal;
                }
            },
            CState::Single => match v {
                b'\n' => {
                    // end of single-line comment
                    // C spec says replace comments with single space
                    self.sink.push(b' ');
                    // also, it's still a newline I guess
                    self.sink.push(b'\n');
                    self.state = CState::Normal;
                }
                _ => {
                    // ignore everything else until newline
                }
            },
            CState::Multi => match v {
                b'*' => {
                    // possible end of multi-line comment
                    self.state = CState::MultiStar;
                }
                _ => {
                    // ignore everything else until `*/`
                }
            },
            CState::MultiStar => match v {
                b'/' => {
                    // end of multi-line comment!
                    // C spec says replace comments with single space
                    self.sink.push(b' ');
                    self.state = CState::Normal;
                }
                v => {
                    // was just a regular star, keep skipping
                    self.state = CState::Multi;
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
