use super::{
    expr::{BinaryOperator as BO, TokenStream},
    Define, DefineArguments, Expr, Include, Punctuator, Token,
};
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
        = name:identifier() value:spaced_token_stream()? {
            Define::Value {
                name,
                value: value.unwrap_or(vec![].into()),
            }
        }

    rule spaced_token_stream() -> TokenStream
        = __ value:token_stream() { value }

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

    pub rule expr() -> Expr
        = e:expr0() eof() { e }

    pub rule expr0() -> Expr = precedence!{
        // precedence 15 (lowest)
        l:(@) _ "||" _ r:@ { l | r }
        --
        // precedence 14
        l:(@) _ "&&" _ r:@ { l & r }
        --
        // precedence 13
        l:(@) _ "|"  _ r:@ { BO::BitwiseOr.build(l, r) }
        --
        // precedence 12
        l:(@) _ "^"  _ r:@ { BO::BitwiseXor.build(l, r) }
        --
        // precedence 11
        l:(@) _ "&"  _ r:@ { BO::BitwiseAnd.build(l, r)  }
        --
        // precedence 10
        l:(@) _ "==" _ r:@ { BO::Equals.build(l, r)  }
        l:(@) _ "!=" _ r:@ { BO::NotEquals.build(l, r)  }
        --
        // precedence 9
        l:(@) _ ">"  _ r:@ { BO::Greater.build(l, r)  }
        l:(@) _ ">=" _ r:@ { BO::GreaterOrEqual.build(l, r)  }
        l:(@) _ "<"  _ r:@ { BO::Less.build(l, r)  }
        l:(@) _ "<=" _ r:@ { BO::LessOrEqual.build(l, r)  }
        --
        // precedence 7
        l:(@) _ "<<" _ r:@ { BO::LeftShift.build(l, r) }
        l:(@) _ ">>" _ r:@ { BO::RightShift.build(l, r) }
        --
        // precedence 6
        l:(@) _ "+"  _ r:@ { BO::Add.build(l, r) }
        l:(@) _ "-"  _ r:@ { BO::Subtract.build(l, r) }
        --
        // precedence 5
        l:(@) _ "*"  _ r:@ { BO::Multiply.build(l, r) }
        l:(@) _ "/"  _ r:@ { BO::Divide.build(l, r) }
        l:(@) _ "*"  _ r:@ { BO::Modulo.build(l, r) }
        --
        // precendence 3
        "!" _ x:@ { Expr::Not(Box::new(x)) }
        --
        "defined" _ name:identifier() { Expr::Defined(name) }
        "defined" _ "(" _ name:identifier() _ ")" { Expr::Defined(name) }
        --
        "true" { Expr::True }
        "false" { Expr::False }
        callee:identifier() _ "(" args:expr0() ** (_ "," _) ")" _ { Expr::Call(callee, args) }
        --
        name:identifier() { Expr::Symbol(name) }
        "(" e:expr0() ")" { e }
        // highest precedence
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

pub trait PreprocessorIdent {
    fn ident(&self) -> Vec<String>;
}

#[cfg(test)]
mod lexer_tests {
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
}

#[cfg(test)]
mod directive_parser_tests {
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

    #[test]
    fn ifdef() {
        assert_eq!(
            parser::directive("#ifdef FOO_BAR"),
            Ok(Some(Directive::If(
                vec![
                    id("defined"),
                    Punctuator::ParenOpen.into(),
                    id("FOO_BAR"),
                    Punctuator::ParenClose.into()
                ]
                .into()
            ))),
        );

        assert_eq!(
            parser::directive("#ifndef FOO_BAR"),
            Ok(Some(Directive::If(
                vec![
                    Punctuator::Bang.into(),
                    id("defined"),
                    Punctuator::ParenOpen.into(),
                    id("FOO_BAR"),
                    Punctuator::ParenClose.into()
                ]
                .into()
            ))),
        );
    }
}

#[cfg(test)]
mod expr_parser_tests {
    use super::{super::expr::BinaryOperator, *};

    fn sym(s: &str) -> Expr {
        Expr::Symbol(s.to_string())
    }

    fn def(s: &str) -> Expr {
        Expr::Defined(s.to_string())
    }

    #[test]
    fn simple_exprs() {
        assert_eq!(parser::expr("azAZ09_"), Ok(sym("azAZ09_")));
        assert!(parser::expr("not$asymbol").is_err());

        assert_eq!(parser::expr("(foobar)"), Ok(sym("foobar")));
        assert_eq!(parser::expr("((foobar))"), Ok(sym("foobar")));
        assert_eq!(parser::expr("true"), Ok(Expr::True));
        assert_eq!(parser::expr("false"), Ok(Expr::False));
        assert_eq!(
            parser::expr("a && b"),
            Ok(Expr::And(vec![sym("a"), sym("b")]))
        );
        assert_eq!(
            parser::expr("a || b"),
            Ok(Expr::Or(vec![sym("a"), sym("b")]))
        );
        assert_eq!(parser::expr("defined foo"), Ok(def("foo")));
        assert_eq!(parser::expr("defined(foo)"), Ok(def("foo")));

        assert_eq!(
            parser::expr("getstuff()"),
            Ok(Expr::Call("getstuff".to_string(), vec![]))
        );
        assert_eq!(
            parser::expr("identity(x)"),
            Ok(Expr::Call("identity".to_string(), vec![sym("x")]))
        );
        assert_eq!(
            parser::expr("add(x, y)"),
            Ok(Expr::Call("add".to_string(), vec![sym("x"), sym("y")]))
        );
        assert!(parser::expr("missing_arg(x, y,)").is_err());
    }

    #[test]
    fn binary_ops() {
        assert_eq!(
            parser::expr("a > b"),
            Ok(Expr::Binary(
                BinaryOperator::Greater,
                Box::new(sym("a")),
                Box::new(sym("b"))
            ))
        );
        assert_eq!(
            parser::expr("a >= b"),
            Ok(Expr::Binary(
                BinaryOperator::GreaterOrEqual,
                Box::new(sym("a")),
                Box::new(sym("b"))
            ))
        );
        assert_eq!(
            parser::expr("a < b"),
            Ok(Expr::Binary(
                BinaryOperator::Less,
                Box::new(sym("a")),
                Box::new(sym("b"))
            ))
        );
        assert_eq!(
            parser::expr("a <= b"),
            Ok(Expr::Binary(
                BinaryOperator::LessOrEqual,
                Box::new(sym("a")),
                Box::new(sym("b"))
            ))
        );
        assert_eq!(
            parser::expr("a == b"),
            Ok(Expr::Binary(
                BinaryOperator::Equals,
                Box::new(sym("a")),
                Box::new(sym("b"))
            ))
        );
        assert_eq!(
            parser::expr("a != b"),
            Ok(Expr::Binary(
                BinaryOperator::NotEquals,
                Box::new(sym("a")),
                Box::new(sym("b"))
            ))
        );
        assert_eq!(
            parser::expr("a | b"),
            Ok(Expr::Binary(
                BinaryOperator::BitwiseOr,
                Box::new(sym("a")),
                Box::new(sym("b"))
            ))
        );
    }

    #[test]
    fn precedence() {
        assert_eq!(
            parser::expr("a && b && c"),
            Ok(Expr::And(vec![sym("a"), sym("b"), sym("c")]))
        );
        assert_eq!(
            parser::expr("a && b || c"),
            Ok(Expr::Or(vec![
                Expr::And(vec![sym("a"), sym("b"),]),
                sym("c")
            ]))
        );
        assert_eq!(
            parser::expr("a || b && c"),
            Ok(Expr::Or(vec![
                sym("a"),
                Expr::And(vec![sym("b"), sym("c")]),
            ]))
        );
        assert_eq!(
            parser::expr("a && b || c && d"),
            Ok(Expr::Or(vec![
                Expr::And(vec![sym("a"), sym("b")]),
                Expr::And(vec![sym("c"), sym("d")]),
            ]))
        );
        assert_eq!(
            parser::expr("a || b && c || d"),
            Ok(Expr::Or(vec![
                sym("a"),
                Expr::And(vec![sym("b"), sym("c")]),
                sym("d"),
            ]))
        );

        assert_eq!(
            parser::expr("foo && bar(baz)"),
            Ok(Expr::And(vec![
                sym("foo"),
                Expr::Call("bar".to_string(), vec![sym("baz")]),
            ]))
        );
        assert_eq!(
            parser::expr("defined foo && bar"),
            Ok(Expr::And(vec![def("foo"), sym("bar"),]))
        );
        assert_eq!(
            parser::expr("foo && defined bar"),
            Ok(Expr::And(vec![sym("foo"), def("bar"),]))
        );
        assert!(parser::expr("defined add(x, y)").is_err())
    }
}
