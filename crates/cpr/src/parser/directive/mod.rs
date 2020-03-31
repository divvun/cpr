use super::{
    expr::{BinaryOperator as BO, TokenStream},
    Define, Expr, Include, MacroParams, Token,
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
                Token::Defined,
                '('.into(),
                Token::Name(i),
                ')'.into(),
            ].into()
        ) }
        / N("ifndef") __ i:identifier() { Directive::If(
            vec![
                Token::Pun('!'),
                Token::Defined,
                '('.into(),
                Token::Name(i),
                ')'.into(),
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
        / t:token_stream() { Include::TokenStream(t) }

    rule define() -> Define
        = define_function_like()
        / define_object_like()

    rule define_function_like() -> Define
        = name:identifier() "(" _ params:macro_params() _ ")" value:spaced_token_stream()? {
            Define::FunctionLike {
                name,
                params,
                value: value.unwrap_or(vec![].into()),
            }
        }

    rule define_object_like() -> Define
        = name:identifier() value:spaced_token_stream()? {
            Define::ObjectLike {
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
        // note: '$' is accepted by MSVC, Clang and GCC in identifiers, see C99 Standard, Annex J, J.5.2
        = n:$(['_' | '$' | 'a'..='z' | 'A'..='Z'] ['_' | '$' | 'a'..='z' | 'A'..='Z' | '0'..='9']*) {
            n.into()
        }
    rule macro_params() -> MacroParams
        = names:identifier() ** (_ "," _) _ e:("," _ "...")? {
            MacroParams {
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
        = __                 { Token::WS }
        / k:tok_operator()   { k }
        / k:tok_string()     { k }
        / k:tok_punctuator() { k }
        / k:identifier()     { Token::Name(k) }
        / k:tok_integer()    { Token::Int(k) }
        / expected!("token")

    rule tok_string() -> Token
        = "\"" n:string_tok()* string_end() { Token::Str(n.join("")) }

    rule string_tok() -> &'input str
        = n:$("\"" _ "\"") { "" }
        / n:$("\\\"")      { n }
        / n:$(!['"'][_])   { n }

    rule string_end()
        = "\""
        / expected!("end of string")

    rule tok_keyword() -> String
        = e:identifier() {?
            if env().reserved.contains(e.as_str()) {
                Ok(e)
            } else {
                Err("not a keyword")
            }
        }

    rule tok_integer() -> i64
        = "0x" s:$(tok_hex_integer()+) {? i64::from_str_radix(&s, 16).map_err(|_| "bad hex constant") }
        / "0" s:$(tok_oct_integer()+) {? i64::from_str_radix(&s, 8).map_err(|_| "bad oct constant") }
        / s:$(tok_dec_integer()+) {? i64::from_str_radix(&s, 10).map_err(|_| "bad decimal constant") }

    rule tok_hex_integer() -> String
        = e:$(['0'..='9' | 'A'..='F' | 'a'..='f']) { e.into() }

    rule tok_oct_integer() -> String
        = e:$(['0'..='7']) { e.into() }

    rule tok_dec_integer() -> String
        = e:$(['0'..='9']) { e.into() }

    rule tok_operator() -> Token
        = "##"      { Token::Paste }
        / "#@"      { Token::Charize }
        / "#"       { Token::Stringize }
        / "defined" { Token::Defined }

    rule tok_punctuator() -> Token
        = s:$([
            '!' | '%' | '^' | '&' | '*' | '(' | ')' | '-' | '+' | '=' |
            '{' | '}' | '|' | '~' | '[' | ']' | '\\'| ';' | ':' | '\''|
            '"' | '<' | '>' | '?' | ',' | '.' | '/' | '@'
          ]) { s.chars().next().unwrap().into() }

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
        i:tok_integer() { Expr::Integer(i) }
        callee:identifier() _ "(" args:expr0() ** (_ "," _) ")" _ { Expr::Call(callee, args) }
        --
        name:identifier() { Expr::Symbol(name) }
        "(" _ e:expr0() _ ")" { e }
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
mod test_lexer;

#[cfg(test)]
mod test_directive_parser;

#[cfg(test)]
mod test_expr_parser;
