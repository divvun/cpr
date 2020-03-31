trait Sink {
    fn push(&mut self, c: char);
}

impl Sink for String {
    fn push(&mut self, c: char) {
        String::push(self, c);
    }
}

/// Line continuation processor
struct LProcessor<'a> {
    state: LState,
    sink: &'a mut dyn Sink,
}

#[derive(Debug)]
enum LState {
    Normal,
    Backslash,
}

impl<'a> Sink for LProcessor<'a> {
    fn push(&mut self, c: char) {
        match self.state {
            LState::Normal => match c {
                '\\' => {
                    self.state = LState::Backslash;
                }
                c => {
                    self.sink.push(c);
                }
            },
            LState::Backslash => match c {
                '\r' => {
                    // ah, CRLF. Ignore and stay in line continuation state
                }
                '\n' => {
                    // line continuation
                    self.state = LState::Normal;
                }
                c => {
                    // not a line continuation, nevermind
                    self.sink.push('\\');
                    self.sink.push(c);
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

#[derive(Debug)]
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
    fn push(&mut self, c: char) {
        match self.state {
            CState::Normal => match c {
                '/' => {
                    self.state = CState::NormalSlash;
                }
                '"' => {
                    self.sink.push('"');
                    self.state = CState::String;
                }
                v => {
                    self.sink.push(v);
                }
            },
            CState::String => match c {
                '"' => {
                    // end of string
                    self.sink.push('"');
                    self.state = CState::Normal;
                }
                '\\' => {
                    // start of escape sequence
                    self.state = CState::StringBackslash;
                }
                c => {
                    self.sink.push(c);
                }
            },
            CState::StringBackslash => match c {
                c => {
                    // this state exists for `\"` pretty much
                    self.sink.push('\\');
                    self.sink.push(c);
                    self.state = CState::String;
                }
            },
            CState::NormalSlash => match c {
                '/' => {
                    // start of single-line comment
                    self.state = CState::Single;
                }
                '*' => {
                    // start of multi-line comment
                    self.state = CState::Multi;
                }
                c => {
                    // just a good old slash
                    self.sink.push('/');
                    self.sink.push(c);
                    self.state = CState::Normal;
                }
            },
            CState::Single => match c {
                '\n' => {
                    // end of single-line comment
                    // C spec says replace comments with single space
                    self.sink.push(' ');
                    // also, it's still a newline I guess
                    self.sink.push('\n');
                    self.state = CState::Normal;
                }
                _ => {
                    // ignore everything else until newline
                }
            },
            CState::Multi => match c {
                '*' => {
                    // possible end of multi-line comment
                    self.state = CState::MultiStar;
                }
                _ => {
                    // ignore everything else until `*/`
                }
            },
            CState::MultiStar => match c {
                '/' => {
                    // end of multi-line comment!
                    // C spec says replace comments with single space
                    self.sink.push(' ');
                    self.state = CState::Normal;
                }
                '*' => {
                    // still in multi-line comment, still possibly ending
                    // multi-line comment
                }
                _ => {
                    // was just a regular star, keep skipping
                    self.state = CState::Multi;
                }
            },
        }
    }
}

/// Given a BufRead, does exactly two things, in this (conceptual) order:
///
///   * Process line continuations, ie. replace "foo\\\nbar" with "foobar"
///   * Strip single-line and multi-line comments, replacing them with a single space
pub fn process_line_continuations_and_comments(input: &str) -> String {
    let mut out = String::new();
    let mut cproc = CProcessor {
        state: CState::Normal,
        sink: &mut out,
    };
    let mut lcproc = LProcessor {
        state: LState::Normal,
        sink: &mut cproc,
    };

    for c in input.chars() {
        lcproc.push(c);
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    fn process_block_comments() {
        let input = indoc!(
            "
            /******
            ******/
            int foobar();
            "
        );
        assert_eq!(
            process_line_continuations_and_comments(&input),
            // comment block is reduced to 'one space'
            " \nint foobar();\n"
        );
    }
}
