use super::{Define, DefineArguments, Expr, Include, Punctuator, Token};
use crate::parser::expr::TokenStream;
use peg::ParseLiteral;

use lang_c::ast::Expression;

fn env() -> lang_c::env::Env {
    let mut env = lang_c::env::Env::with_core();
    env.ignore_reserved(true);
    env.reserved.insert("defined");
    env
}

peg::parser! { pub(crate) grammar parser() for str {
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
        / N("if") __ t:token_stream() { Directive::If(t) }
        / N("elif") __ t:token_stream() { Directive::ElseIf(t) }
        / N("ifdef") __ i:identifier() { Directive::If(
            vec![
                Token::Identifier("defined".into()),
                Punctuator::ParenOpen.into(),
                Token::Identifier(i),
                Punctuator::ParenClose.into(),
            ].into()
        ) }
        / N("ifndef") __ i:identifier() { Directive::If(
            vec![
                Punctuator::Bang.into(),
                Token::Identifier("defined".into()),
                Punctuator::ParenOpen.into(),
                Token::Identifier(i),
                Punctuator::ParenClose.into(),
            ].into()
        ) }
        / N("else") eof() { Directive::Else }
        / N("endif") eof() { Directive::EndIf }
        / N("define") __ d:define() { Directive::Define(d) }
        / N("undef") __ n:undef() { Directive::Undefine(n) }
        / N("error") __ s:$([_]*) { Directive::Error(s.into()) }
        / N("pragma") __ s:$([_]*) { Directive::Error(s.into()) }
        / l:$(![' '][_]+) __ r:$([_]*)  { Directive::Unknown(l.into(), r.into()) }
        / expected!("directive name")

    rule undef() -> String
        = n:identifier() eof() { n }

    rule include() -> Include
        = t:include_token() eof() { t }

    rule include_token() -> Include
        = "<" p:$((!['>'][_])+) ">" { Include::System(p.into()) }
        / "\"" p:$((!['"'][_])+) "\"" { Include::Quoted(p.into()) }
        / e:constant_expression() { Include::Expression(e) }

    rule define() -> Define
        = define_function_like()
        / define_object_like()

    rule define_function_like() -> Define
        = name:identifier() "(" _ args:identifier_list() _ ")" __ value:token_stream() {
            Define::Replacement {
                name,
                args,
                value,
            }
        }

    rule define_object_like() -> Define
        = name:identifier() __ value:token_stream() {
            Define::Value {
                name,
                value,
            }
        }

    rule eof()
        = _ ![_] // 0+ whitespace then eof
        / expected!("eof")

    rule identifier() -> String
        = n:$(['_' | 'a'..='z' | 'A'..='Z'] ['_' | 'a'..='z' | 'A'..='Z' | '0'..='9']*) {
            n.into()
        }
    rule identifier_list() -> DefineArguments
        = names:identifier() ** (_ "," _) _ e:("," _ "...")? {
            DefineArguments {
                names,
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

    pub rule token_stream() -> TokenStream
        = t:token()* eof() { t.into() }

    pub rule token() -> Token
        = __ { Token::Whitespace }
        / k:tok_punctuator() { Token::Punctuator(k) }
        / k:tok_keyword() { Token::Keyword(k) }
        / k:identifier() { Token::Identifier(k) }
        / k:tok_integer() { Token::Integer(k) }
        / expected!("token")

    rule tok_keyword() -> String
        = e:identifier() {?
            if env().reserved.contains(e.as_str()) {
                Ok(e)
            } else {
                Err("not a keyword")
            }
        }

    rule tok_integer() -> i64
        = "0x" s:tok_hex_integer() {? i64::from_str_radix(&s, 16).map_err(|_| "bad hex constant") }
        / "0" s:tok_oct_integer() {? i64::from_str_radix(&s, 8).map_err(|_| "bad oct constant") }
        / s:tok_dec_integer() {? i64::from_str_radix(&s, 10).map_err(|_| "bad decimal constant") }

    rule tok_hex_integer() -> String
        = e:$(['0'..='9' | 'A'..='F' | 'a'..='f']) { e.into() }

    rule tok_oct_integer() -> String
        = e:$(['0'..='7']) { e.into() }

    rule tok_dec_integer() -> String
        = e:$(['0'..='9']) { e.into() }

    rule tok_punctuator() -> Punctuator
        = "!" { Punctuator::Bang }
        / "%" { Punctuator::Percent }
        / "^" { Punctuator::Circumflex }
        / "&" { Punctuator::Ampersand }
        / "*" { Punctuator::Star }
        / "(" { Punctuator::ParenOpen }
        / ")" { Punctuator::ParenClose }
        / "-" { Punctuator::Minus }
        / "+" { Punctuator::Plus }
        / "=" { Punctuator::Equal }
        / "{" { Punctuator::CurlyOpen }
        / "}" { Punctuator::CurlyClose }
        / "|" { Punctuator::Pipe }
        / "~" { Punctuator::Tilde }
        / "[" { Punctuator::SquareOpen }
        / "]" { Punctuator::SquareClose }
        / "\\" { Punctuator::Backslash }
        / ";" { Punctuator::Semicolon }
        / "'" { Punctuator::SingleQuote }
        / ":" { Punctuator::Colon }
        / "\"" { Punctuator::DoubleQuote }
        / "<" { Punctuator::AngleOpen }
        / ">" { Punctuator::AngleClose }
        / "?" { Punctuator::Question }
        / "," { Punctuator::Comma }
        / "." { Punctuator::Dot }
        / "/" { Punctuator::Slash }
        / "#" { Punctuator::Hash }

    pub rule expr() -> Expr = precedence!{
        l:(@) _ "|" _ r:@ { l | r }
        --
        l:(@) _ "&" _ r:@ { l & r }
        --
        name:identifier() { Expr::Symbol(name) }
        callee:@ _ "(" args:expr() ** (_ "," _) ")" _ { Expr::Call(Box::new(callee), args) }
        --
        "(" e:expr() ")" { e }
        --
        "defined" _ name:identifier() { Expr::Defined(name) }
        "defined" _ "(" _ name:identifier() _ ")" { Expr::Defined(name) }
    }
}}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Directive {
    If(TokenStream),
    Else,
    ElseIf(TokenStream),
    EndIf,
    Include(Include),
    Define(Define),
    Undefine(String),
    Error(String),
    Pragma(String),
    Unknown(String, String),
}

#[cfg(test)]
mod tests {
    use super::*;

    use Punctuator as P;
    use Token as T;
    use T::Whitespace as __;

    fn id(s: &str) -> T {
        T::Identifier(s.into())
    }

    fn int(i: i64) -> T {
        T::Integer(i)
    }

    #[test]
    fn tokens() {
        assert_eq!(
            parser::token_stream("2 + 4"),
            Ok(vec![int(2), __, P::Plus.into(), __, int(4)].into())
        );

        assert_eq!(
            parser::token_stream("f(x) = y;"),
            Ok(vec![
                id("f"),
                P::ParenOpen.into(),
                id("x"),
                P::ParenClose.into(),
                __,
                P::Equal.into(),
                __,
                id("y"),
                P::Semicolon.into(),
            ]
            .into())
        );
    }

    #[test]
    fn not_a_directive() {
        assert_eq!(parser::directive(""), Ok(None));
        assert_eq!(parser::directive("int foobar();"), Ok(None));
    }

    #[test]
    fn define_objectlike_empty() {
        assert_eq!(
            parser::directive("#define FOO"),
            Ok(Some(Directive::Define(Define::Value {
                name: "FOO".into(),
                value: vec![].into()
            })))
        );
    }

    #[test]
    fn define_objectlike() {
        assert_eq!(
            parser::directive("#define FOO BAR"),
            Ok(Some(Directive::Define(Define::Value {
                name: "FOO".into(),
                value: vec![id("BAR")].into()
            })))
        );
    }

    #[test]
    fn define_objectlike_2() {
        assert_eq!(
            parser::directive("#define FOO BAR(BAZ)"),
            Ok(Some(Directive::Define(Define::Value {
                name: "FOO".into(),
                value: vec![
                    id("BAR"),
                    P::ParenOpen.into(),
                    id("BAZ"),
                    P::ParenClose.into(),
                ]
                .into()
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
                    names: vec!["X".into(), "Y".into()],
                    has_trailing: false
                },
                value: vec![id("X"), __, P::Plus.into(), __, id("Y")].into()
            })))
        );
    }

    #[test]
    fn include_angle() {
        assert_eq!(
            parser::directive("#include <foo/bar/baz.h>"),
            Ok(Some(Directive::Include(Include::System(
                "foo/bar/baz.h".into()
            ))))
        )
    }

    #[test]
    fn include_quoted() {
        assert_eq!(
            parser::directive(r#"#include "shared/um/sure.h""#),
            Ok(Some(Directive::Include(Include::Quoted(
                "shared/um/sure.h".into()
            ))))
        )
    }
}

pub trait PreprocessorIdent {
    fn ident(&self) -> Vec<String>;
}
