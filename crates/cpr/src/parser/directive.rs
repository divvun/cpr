use std::convert::TryFrom;

use super::{Define, DefineArguments, Include};

use lang_c::ast::Expression;
use regex::Regex;

fn env() -> lang_c::env::Env {
    let mut env = lang_c::env::Env::with_core();
    env.ignore_reserved(true);
    env.reserved.insert("defined");
    // env.single_line_mode(true);
    env
}

peg::parser! { pub(crate) grammar parser() for str {
    use peg::ParseLiteral;

    /// 0+ whitespace
    rule _()
        = quiet!{[' ' | '\t']*}
    /// 1+ whitespace
    rule __()
        = quiet!{[' ' | '\t']+}
    /// matches #name literally
    rule H(name: &str)
        = ("#" _ ##parse_string_literal(name))
    /// matches name literally
    rule N(name: &str)
        = ##parse_string_literal(name)

    /// Parses line (no CR/LF, no comments, no line continuations) as directive or None
    pub rule directive() -> Option<Directive>
        = _ d:directive0() { d }

    rule directive0() -> Option<Directive>
        = "#" _ d:directive1() { Some(d) }
        / eof() { None } // empty line
        / !"#" [_]* eof() { None } // non-directive line

    rule directive1() -> Directive
        = N("include") __ i:include() { Directive::Include(i) }
        / N("define") __ d:define() { Directive::Define(d) }
        / l:$(![' '][_]+) __ r:$([_]*)  { Directive::Unknown(l.into(), r.into()) }
        / expected!("directive name")

    rule include() -> Include
        = t:include_token() eof() { t }

    pub rule include_line() -> Include
        = H("include") __ t:include_token() eof() { t }

    rule define() -> Define
        = define_function_like()
        / define_object_like()

    rule define_function_like() -> Define
        = name:identifier() "(" _ args:identifier_list() _ ")" __ value:replacement_list() eof() {
            Define::Replacement {
                name,
                args,
                value: value.join(" "),
            }
        }

    rule define_object_like() -> Define
        = name:identifier() __ value:replacement_list()? eof() {
            Define::Value {
                name,
                value: value.map(|x| x.join(" ")),
            }
        }

    pub rule define_line() -> Define
        = expected!("deprecated")
    rule undef_line()
        = H("undef") __ identifier() eof()
        / expected!("#undef")
    rule line_line()
        = H("line") __ t:$([_]+) eof()
        / expected!("#line")
    rule error_line()
        = H("error") (__ t:$([_]+))? eof()
        / expected!("#error")
    rule pragma_line()
        = H("pragma") (__ t:$([_]+))? eof()
        / expected!("#pragma")
    rule include_token() -> Include
        = "<" p:$((!['>'][_])+) ">" { Include::System(p.into()) }
        / "\"" p:$((!['"'][_])+) "\"" { Include::Quoted(p.into()) }
        / e:constant_expression() { Include::Expression(e) }

    /// used for macro bodies and define values
    rule replacement_list() -> Vec<String>
        = n:$(![' '][_]+) ** (_ " " _) { n.iter().map(|x| x.to_string()).collect() }
    rule eof()
        = _ ![_] // 0+ whitespace then eof
        / expected!("eof")
    rule identifier() -> String
        = n:$(['_' | 'a'..='z' | 'A'..='Z'] ['_' | 'a'..='z' | 'A'..='Z' | '0'..='9']*) {
            n.into()
        }
    rule identifier_list() -> DefineArguments
        = i:identifier() ** (_ "," _) _ e:("," _ "...")? {
            DefineArguments {
                values: i,
                has_trailing: e.is_some(),
            }
        }
    rule constant_expression() -> Expression
        = e:$((!['\n'][_])+) {?
            match lang_c::parser::constant_expression(e, &mut env()) {
                Ok(v) => Ok(v.node),
                Err(err) => {
                    log::error!("{}", err);
                    log::info!("{:?}", e);
                    Err("constant expression")
                }
            }
        }
}}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Directive {
    If(Expression),
    Else,
    ElseIf(Expression),
    EndIf,
    IfDefined(String),
    IfNotDefined(String),
    Include(Include),
    Define(Define),
    Undefine(String),
    Error(String),
    Pragma(String),
    Unknown(String, String),
}

fn workaround_braceless_defined(value: &str) -> String {
    lazy_static::lazy_static! {
        static ref BRACELESS_DEFINED: Regex = Regex::new(r"defined ([^\s]+)").unwrap();
        static ref DOUBLESLASH_COMMENT: Regex = Regex::new("// .*$").unwrap();
        static ref SLASHSTAR_COMMENT: Regex = Regex::new(r"/\*.*?\*/").unwrap();
    }
    let v = BRACELESS_DEFINED.replace_all(value, "defined($1)");
    let v = DOUBLESLASH_COMMENT.replace_all(&v, "");
    SLASHSTAR_COMMENT.replace_all(&v, "").to_string()
}

pub(crate) fn parse_directive(line: &str) -> Option<Directive> {
    lazy_static::lazy_static! {
        static ref DIRECTIVE_PATTERN: Regex = Regex::new(r"^\s*#\s*([^\s]+?)(?:\s(.*?))?\s*(?:\s*//.*)?$")
            .expect("regex must always be valid");
    }

    let captures = match DIRECTIVE_PATTERN.captures(line) {
        Some(v) => v,
        None => return None,
    };

    let key = match captures.get(1).map(|x| x.as_str()) {
        Some(v) => v,
        None => return None,
    };

    let value = match captures.get(2).map(|x| x.as_str()) {
        Some(v) => workaround_braceless_defined(&v).trim().to_string(),
        None => "".to_string(),
    };

    use Directive::*;
    match key {
        "if" => match lang_c::parser::constant_expression(&value, &mut env()) {
            Ok(v) => match Expression::try_from(v.node) {
                Ok(expr) => Some(If(expr)),
                Err(e) => {
                    dbg!(e);
                    panic!(e)
                }
            },
            Err(e) => {
                dbg!(e);
                panic!("if constant expression: {:?}", value)
            }
        },
        "elif" => match lang_c::parser::constant_expression(&value, &mut env()) {
            Ok(v) => match Expression::try_from(v.node) {
                Ok(expr) => Some(ElseIf(expr)),
                Err(e) => {
                    dbg!(e);
                    panic!(e)
                }
            },
            Err(e) => {
                dbg!(e);
                panic!("elif constant expression: {:?}", value)
            }
        },
        "else" => Some(Else),
        "endif" => Some(EndIf),
        "ifdef" => Some(IfDefined(value)),
        "ifndef" => Some(IfNotDefined(value)),
        "include" => parser::include_line(&format!("#include {}", value))
            .map(Include)
            .map_err(|_| ())
            .ok(),
        "define" => parser::define_line(&format!("#define {}", value))
            .map(Define)
            .map_err(|e| {
                dbg!(&e);
                ()
            })
            .ok(),
        "undef" => Some(Undefine(value)),
        "error" => Some(Error(value)),
        "pragma" => Some(Pragma(value)),
        _ => Some(Unknown(key.to_string(), value)),
    }
}

#[cfg(test)]
mod tests {
    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn not_a_directive() {
        assert_eq!(parser::directive(""), Ok(None));
        assert_eq!(parser::directive("int foobar();"), Ok(None));
    }

    #[test]
    fn define_objectlike() {
        assert_eq!(
            parser::directive("#define FOO BAR"),
            Ok(Some(Directive::Define(Define::Value {
                name: "FOO".into(),
                value: Some("BAR".into())
            })))
        );
    }

    #[test]
    fn define_objectlike_2() {
        assert_eq!(
            parser::directive("#define FOO BAR(BAZ)"),
            Ok(Some(Directive::Define(Define::Value {
                name: "FOO".into(),
                value: Some("BAR(BAZ)".into())
            })))
        );
    }

    #[test]
    fn define_functionlike_1() {
        assert_eq!(
            parser::directive("#define FOO(X, Y) X + Y"),
            Ok(Some(Directive::Define(Define::Replacement {
                name: "FOO".into(),
                args: DefineArguments {
                    values: vec!["X".into(), "Y".into()],
                    has_trailing: false
                },
                value: "X + Y".into()
            })))
        );
    }
}

pub trait PreprocessorIdent {
    fn ident(&self) -> Vec<String>;
}

impl<T: PreprocessorIdent> PreprocessorIdent for lang_c::span::Node<T> {
    fn ident(&self) -> Vec<String> {
        self.node.ident()
    }
}

impl PreprocessorIdent for lang_c::ast::Identifier {
    fn ident(&self) -> Vec<String> {
        vec![self.name.clone()]
    }
}

impl PreprocessorIdent for lang_c::ast::UnaryOperatorExpression {
    fn ident(&self) -> Vec<String> {
        self.operand.ident()
    }
}

impl PreprocessorIdent for lang_c::ast::BinaryOperatorExpression {
    fn ident(&self) -> Vec<String> {
        let mut vec = vec![];
        vec.append(&mut self.lhs.ident());
        vec.append(&mut self.rhs.ident());
        vec
    }
}

impl PreprocessorIdent for lang_c::ast::ConditionalExpression {
    fn ident(&self) -> Vec<String> {
        let mut vec = vec![];
        vec.append(&mut self.condition.ident());
        vec.append(&mut self.then_expression.ident());
        vec.append(&mut self.else_expression.ident());
        vec
    }
}

impl PreprocessorIdent for lang_c::ast::CallExpression {
    fn ident(&self) -> Vec<String> {
        let mut vec = vec![];
        // vec.append(&mut self.callee.ident());
        vec.append(&mut self.arguments.iter().map(|x| x.ident()).flatten().collect());
        vec
    }
}

impl PreprocessorIdent for lang_c::ast::Expression {
    fn ident(&self) -> Vec<String> {
        use lang_c::ast::Expression::*;
        match self {
            Identifier(x) => x.ident(),
            Call(x) => x.ident(),
            UnaryOperator(x) => x.ident(),
            BinaryOperator(x) => x.ident(),
            Conditional(x) => x.ident(),
            _ => vec![],
        }
    }
}
