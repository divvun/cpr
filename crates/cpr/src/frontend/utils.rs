use super::LineNo;

trait Sink {
    fn push(&mut self, c: char);
}

trait MiddleSink: Sink {
    fn set_lineno(&mut self, lineno: LineNo);
}

struct LinesSink {
    lines: Vec<(LineNo, String)>,
    current_line: String,
    current_lineno: LineNo,
}

impl LinesSink {
    fn new() -> Self {
        Self {
            lines: vec![],
            current_line: Default::default(),
            current_lineno: LineNo(1),
        }
    }
}

impl Sink for LinesSink {
    fn push(&mut self, c: char) {
        if c == '\n' {
            let mut l = "".into();
            std::mem::swap(&mut l, &mut self.current_line);
            self.lines.push((self.current_lineno, l));
        } else {
            self.current_line.push(c);
        }
    }
}

impl MiddleSink for LinesSink {
    fn set_lineno(&mut self, lineno: LineNo) {
        self.current_lineno = lineno;
    }
}

struct LineCounter<'a> {
    lineno: LineNo,
    sink: &'a mut dyn MiddleSink,
}

impl<'a> LineCounter<'a> {
    fn new(sink: &'a mut dyn MiddleSink) -> Self {
        let lineno = LineNo(0);
        sink.set_lineno(lineno);
        Self { lineno, sink }
    }
}

impl<'a> Sink for LineCounter<'a> {
    fn push(&mut self, c: char) {
        if c == '\n' {
            self.lineno = LineNo(self.lineno.0 + 1);
            self.sink.set_lineno(self.lineno);
        }
        self.sink.push(c);
    }
}

/// Line continuation processor
struct LProcessor<'a> {
    state: LState,
    sink: &'a mut dyn MiddleSink,
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
                '\r' => {
                    // ignore the CR in CRLF
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

impl<'a> MiddleSink for LProcessor<'a> {
    fn set_lineno(&mut self, lineno: LineNo) {
        self.sink.set_lineno(lineno);
    }
}

/// Comment processor
struct CProcessor<'a> {
    state: CState,
    sink: &'a mut dyn MiddleSink,
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

impl<'a> MiddleSink for CProcessor<'a> {
    fn set_lineno(&mut self, lineno: LineNo) {
        self.sink.set_lineno(lineno)
    }
}

/// Given a BufRead, does exactly two things, in this (conceptual) order:
///
///   * Process line continuations, ie. replace "foo\\\nbar" with "foobar"
///   * Strip single-line and multi-line comments, replacing them with a single space
pub fn process_line_continuations_and_comments(input: &str) -> Vec<(LineNo, String)> {
    let mut ls = LinesSink::new();

    let mut cproc = CProcessor {
        state: CState::Normal,
        sink: &mut ls,
    };
    let mut lproc = LProcessor {
        state: LState::Normal,
        sink: &mut cproc,
    };
    let mut lc = LineCounter::new(&mut lproc);

    for c in input.chars() {
        lc.push(c);
    }
    // add final newline if the input didn't have one
    if !input.ends_with("\n") {
        lc.push('\n');
    }

    ls.lines
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
            vec![(LineNo(2), " ".into()), (LineNo(3), "int foobar();".into())],
        );
    }
}
