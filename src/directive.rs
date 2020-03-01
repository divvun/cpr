use std::convert::TryFrom;

use super::{Define, Include};

use lang_c::ast::{Expression, Identifier};
use regex::Regex;

struct Identifiers {
    values: Vec<Identifier>,
    has_trailing: bool,
}

fn env() -> lang_c::env::Env {
    let mut env = lang_c::env::Env::with_core();
    env.ignore_reserved(true);
    env.reserved.insert("defined");
    // env.single_line_mode(true);
    env
}

peg::parser! { pub(crate) grammar parser() for str {
    use peg::ParseLiteral;

    rule _()
        = quiet!{[' ' | '\t']*}
    rule __()
        = quiet!{[' ' | '\t']+}
    rule H(name: &str)
        = ("#" _ ##parse_string_literal(name))
    pub rule group()
        = group_part()*
    rule group_part()
        = if_section()
        / control_line()
        / text_line()
    rule if_section()
        = if_group()
        / elif_group()+
        / else_group()
        / endif_line()
        / expected!("if section")
    rule if_group()
        = H("if") __ constant_expression() _ new_line()
        / H("ifdef") __ identifier() _ new_line()
        / H("ifndef") __ identifier() _ new_line()
        / expected!("if group")
    rule elif_group()
        = H("elif") __ constant_expression() _ new_line()
    rule else_group()
        = H("else") _ new_line()
    rule endif_line()
        = H("endif") _ new_line()
    pub rule include_line() -> Include
        = H("include") __ t:include_token() _ new_line()? { t }
    rule define_line0() -> Define
        = H("define") __ i:identifier() __ r:replacement_list()? _ new_line() {
            Define::Value { name: i.name, value: r.map(|x| x.join(" ")) }
        }
    rule define_line1() -> Define
        = H("define") __ i:identifier() "(" _ a:identifier_list() _ ")" __ r:replacement_list() new_line() {
            Define::Replacement {
                name: i.name,
                args: a.values.into_iter().map(|x| x.name).collect(),
                value: r.join(" ")
            }
        }
    pub rule define_line() -> Define
        = define_line0()
        / define_line1()
    rule undef_line()
        = H("undef") __ identifier() _ new_line()
        / expected!("#undef")
    rule line_line()
        = H("line") __ t:$([_]+) _ new_line()
        / expected!("#line")
    rule error_line()
        = H("error") (__ t:$([_]+))? _ new_line()
        / expected!("#error")
    rule pragma_line()
        = H("pragma") (__ t:$([_]+))? _ new_line()
        / expected!("#pragma")
    rule control_line()
        = include_line()
        / define_line0()
        / define_line1()
        / undef_line()
        / line_line()
        / error_line()
        / pragma_line()
        / ("#" _ new_line())
        / expected!("control line")
    rule include_token() -> Include
        = "<" p:$((!['>'][_])+) ">" { Include::System(p.into()) }
        / "\"" p:$((!['"'][_])+) "\"" { Include::Quoted(p.into()) }
        / e:constant_expression() { Include::Expression(e) }

    rule text_line()
        = [_]* new_line()
    rule non_directive()
        = [_]+ new_line()
    rule replacement_list() -> Vec<String>
        = n:$(![' '][_]+) ** (_ " " _) { n.iter().map(|x| x.to_string()).collect() }
    rule new_line()
        = quiet!{"\n"}?
        / expected!("newline")
        // / EOF()
    rule identifier() -> Identifier
        = e:$(!['\n'][_]+) {?
            match lang_c::parser::identifier(e, &mut env()) {
                Ok(v) => Ok(v.node),
                Err(e) => Err("identifier")
            }
        }
    rule identifier_list() -> Identifiers
        = i:identifier() ** (_ "," _) _ e:("," _ "...")? {
            Identifiers {
                values: i,
                has_trailing: e.is_some(),
            }
        }
    rule constant_expression() -> Expression
        = e:$((!['\n'][_])+) {?
            match lang_c::parser::constant_expression(e, &mut env()) {
                Ok(v) => Ok(v.node),
                Err(e) => { dbg!(e); Err("constant expression") }
            }
        }
}}

#[derive(Debug, Clone)]
pub(crate) enum Directive {
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
    let regex = Regex::new(r"defined ([^\s]+)").unwrap();
    let regex2 = Regex::new("// .*$").unwrap();
    let v = regex.replace_all(value, "defined($1)");
    regex2.replace_all(&v, "").to_string()
}

pub(crate) fn parse_directive(line: &str) -> Option<Directive> {
    let regex = Regex::new(r"^\s*#\s*([^\s]+?)(?:\s(.*?))?\s*(?:\s*//?.*)?$")
        .expect("regex must always be valid");
    let captures = match regex.captures(line) {
        Some(v) => v,
        None => return None,
    };

    let key = match captures.get(1).map(|x| x.as_str()) {
        Some(v) => v,
        None => return None,
    };

    let value = match captures.get(2).map(|x| x.as_str()) {
        Some(v) => v.to_string(),
        None => "".to_string(),
    };

    use Directive::*;
    match key {
        "if" => {
            let value = workaround_braceless_defined(&value);
            match lang_c::parser::constant_expression(&value, &mut env()) {
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
            }
        }
        "elif" => {
            let value = workaround_braceless_defined(&value);
            match lang_c::parser::constant_expression(&value, &mut env()) {
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
            }
        }
        "else" => Some(Else),
        "endif" => Some(EndIf),
        "ifdef" => Some(IfDefined(value)),
        "ifndef" => Some(IfNotDefined(value)),
        "include" => parser::include_line(&format!("#include {}", &value))
            .map(Include)
            .map_err(|_| ())
            .ok(),
        "define" => parser::define_line(&value).map(Define).map_err(|_| ()).ok(),
        "undef" => Some(Undefine(value)),
        "error" => Some(Error(value)),
        "pragma" => Some(Pragma(value)),
        _ => Some(Unknown(key.to_string(), value)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test1() {
        println!("{:?}", parser::group("#if 42 == 42\n#endif\n"));
        println!("{:?}", parser::group("#if 23\n#elif 32 == 42\n#endif\n"));
    }

    #[test]
    fn test_call() {
        let x = "#if EXTERNAL_REQ(VAL1 | VAL2)\n";
        println!("{:?}", parser::group(x));
    }

    #[test]
    fn test_call2() {
        let x = "#if EXTERNAL_REQ(VAL2) || EXTERNAL_REQ2(VAL1) && lowercase_req(val3)\n";
        println!("{:?}", parser::group(x));
    }

    #[test]
    fn test_include1() {
        let x = "#include <lol.h>\n";
        println!("{:?}", parser::group(x));
    }

    #[test]
    fn test_include2() {
        let x = "#include \"lol.h\"\n";
        println!("{:?}", parser::group(x));
    }
}
