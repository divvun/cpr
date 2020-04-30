//! C language parser and abstract syntax tree

pub mod ast;
pub mod env;
pub mod span;
pub mod visit;

mod astutil;
mod strings;

// TODO: re-enable in some form
// #[cfg(test)]
// mod tests;

pub mod parser_types {
    use crate::ast::*;
    use crate::span::Node;

    pub type Ds = Vec<Node<DeclarationSpecifier>>;
    pub type Id = Vec<Node<InitDeclarator>>;
}

peg::parser! { pub grammar c_parser(env: &env::ParserEnv<'_>) for str {

use ast::*;
use astutil::*;
use span::{Node, Span};
use env::Symbol;
use super::parser_types::*;

////
// Prologue
////

rule box<T>(ex: rule<T>) -> Box<T> = e:ex() { Box::new(e) }

rule node<T>(ex: rule<T>) -> Node<T> = l:position!() e:ex() r:position!() { Node::new(e, Span::span(l, r)) }

// Lists of elements.
rule list0<T>(ex: rule<T>) -> Vec<T> = e:(ex() ** _) { e }
rule list1<T>(ex: rule<T>) -> Vec<T> = e:(ex() ++ _) { e }
rule cs0<T>(ex: rule<T>) -> Vec<T> = e:(ex() ** (_ "," _)) { e }
rule cs1<T>(ex: rule<T>) -> Vec<T> = e:(ex() ++ (_ "," _)) { e }

// A list containing 0+ before's, 1 single, and 0+ after's
rule list_010<T>(before: rule<T>, single: rule<T>, after: rule<T>) -> Vec<T>
    = before:list0(<before()>) _ single:single() _ after:list0(<after()>) {
        let mut before = before;
        before.push(single);
        before.extend(after);
        before
    }
// A list containing *exactly* one element of a, and any of b.
rule list_eq1_n<T>(a: rule<T>, b: rule<T>) -> Vec<T>
    = list_010(<b()>, <a()>, <b()>)
// A list containing *at least* one element of a, and any of b.
rule list_ge1_n<T>(a: rule<T>, b: rule<T>) -> Vec<T>
    = list_010(<b()>, <a()>, <a() / b()>)

////
// Whitespace
////

rule _() = quiet!{ ['\n' | '\t' | ' ']* }

////
// 6.4.1 Keywords
////

rule K<R>(r: rule<R>) -> R = quiet!{ e:r() !['_' | 'a'..='z' | 'A'..='Z' | '0'..='9'] { e } }

////
// 6.4.2 Identifiers
////

// Identifiers.
pub rule identifier() -> Node<Identifier> = node(<identifier0()>)

rule identifier0() -> Identifier =
    n:$(['_' | 'a'..='z' | 'A'..='Z'] ['_' | 'a'..='z' | 'A'..='Z' | '0'..='9']*) {?
        if !env.get().reserved.contains(n) {
            Ok(Identifier {
                name: n.into(),
            })
        } else {
            Err("identifier")
        }
    }

////
// 6.4.3 Universal character names
////

// TODO

////
// 6.4.4 Constants
////

rule ohx() = "0" ['x' | 'X']
rule dec() = ['0'..='9']
rule oct() = ['0'..='7']
rule hex() = ['0'..='9' | 'A'..='F' | 'a'..='f']

pub rule constant() -> Constant =
    &['0'..='9' | '.'] c:numeric_constant() { c } /
    &['\'' | 'u' | 'U' | 'L'] c:character_constant() { Constant::Character(c) }

rule numeric_constant() -> Constant =
    c:float_constant() { Constant::Float(c) } /
    c:integer_constant() { Constant::Integer(c) }

rule integer_constant() -> Integer =
    n:integer_number() suffix:integer_suffix() {
        let (base, number) = n;
        Integer {
            base: base,
            number: number.to_owned().into_boxed_str(),
            suffix: suffix,
        }
    }

rule integer_number() -> (IntegerBase, &'input str) =
    n:$(['1'..='9'] dec()*) { (IntegerBase::Decimal, n) } /
    ohx() n:$(hex()+) { (IntegerBase::Hexadecimal, n) } /
    "0" n:$(oct()+) { (IntegerBase::Octal, n) } /
    n:$("0") { (IntegerBase::Decimal, n) }

rule integer_suffix() -> IntegerSuffix =
    quiet!{integer_suffix_inner()} / expected!("integer suffix")

rule integer_suffix_inner() -> IntegerSuffix =
    s:$(['u'|'U'|'l'|'L'] / gnu(<['i' | 'I' | 'j' | 'J']>) *) {? int_suffix(s) }

rule float_constant() -> Float =
    n:float_number() suffix:float_suffix() {
        let (base, number) = n;
        Float {
            base: base,
            number: number.to_string().into_boxed_str(),
            suffix: suffix,
        }
    }

rule float_number() -> (FloatBase, &'input str) =
    n:$(float_decimal()) { (FloatBase::Decimal, n) } /
    ohx() n:$(float_hexadecimal()) { (FloatBase::Hexadecimal, n) }

rule float_decimal() =
    dec()* "." dec()+ float_decimal_exp()? /
    dec()+ "." float_decimal_exp()? /
    dec()+ float_decimal_exp()

rule float_decimal_exp() = ['e' | 'E']['+' | '-']?dec()+

rule float_hexadecimal() =
    hex()* "." hex()+ float_binary_exp() /
    hex()+ "." float_binary_exp() /
    hex()+ float_binary_exp()

rule float_binary_exp() = ['p' | 'P']['+' | '-']?dec()+

rule float_suffix() -> FloatSuffix = quiet!{ float_suffix_inner() } / expected!("float literal suffix")

rule float_suffix_inner() -> FloatSuffix =
    gnu(<['i' | 'I' | 'j' | 'J']>)  fmt:float_format() {
        FloatSuffix {
            format: fmt,
            imaginary: true,
        }
    } /
    fmt:float_format() imag:gnu(<['i' | 'I' | 'j' | 'J']>)? {
        FloatSuffix {
            format: fmt,
            imaginary: imag.is_some(),
        }
    }

rule float_format() -> FloatFormat =
    f:ts18661_float_suffix() { FloatFormat::TS18661Format(f) } /
    ['f' | 'F'] { FloatFormat::Float } /
    ['l' | 'L'] { FloatFormat::LongDouble } /
    { FloatFormat::Double }

rule character_constant() -> String =
    c:$(['L' | 'u' | 'U']? "'" character()+ "'") { String::from(c) }

rule character() = !['\\' | '\'' | '\n'] / escape_sequence()

rule escape_sequence() = "\\" (['\'' | '?' | '\\' | 'a'..='c' | 'f' | 'n' | 'r' | 't' | 'v'] / oct()*<1,3> / "x" hex()+)

////
// 6.4.5 String literal
////

pub rule string_literal() -> Node<Vec<String>>
    = s:node(< list1(<string_literal0()>) >) { s }

rule string_literal0() -> String =
    s:$(encoding_prefix()? "\"" string_char()* "\"") { String::from(s) }

rule encoding_prefix() = "u8" / ['u' | 'U' | 'L']

rule string_char() = ['"' | '\\' | '\n'] / escape_sequence()

////
// 6.5.1 Primary expression
////

rule primary_expression() -> Box<Node<Expression>> = box(<node(<primary_expression0()>)>)

rule primary_expression0() -> Expression =
    a:identifier() { Expression::Identifier(Box::new(a)) } /
    a:node(<constant()>) { Expression::Constant(Box::new(a)) } /
    a:string_literal() { Expression::StringLiteral(Box::new(a)) } /
    "(" _ a:expression0() _ ")" { a } /
    a:node(<generic_selection()>) { Expression::GenericSelection(Box::new(a)) } /
    gnu(<gnu_primary_expression()>)

rule generic_selection() -> GenericSelection =
    K(<"_Generic">) _ "(" _ e:assignment_expression() _ "," _ a:cs1(<node(<generic_association()>)>) _ ")" {
        GenericSelection {
            expression: e,
            associations: a,
        }
    }

rule generic_association() -> GenericAssociation =
    t:type_name() _ ":" _ e:assignment_expression() {
        let span = Span::span(t.span.start, e.span.end);
        GenericAssociation::Type(Node::new(GenericAssociationType {
            type_name: t,
            expression: e,
        }, span))
    } /
    K(<"default">) _ ":" _ e:assignment_expression() {
        GenericAssociation::Default(e)
    }

//// 6.5.2 Postfix operators

rule postfix_expression() -> Box<Node<Expression>> = box(<node(<postfix_expression0()>)>)

#[cache]
rule postfix_expression0() -> Expression =
    e:node(<postfix_expression1()>) _ t:list0(<node(<postfix_expressionT()>)>) { apply_ops(t, e).node }

rule postfix_expression1() -> Expression =
    primary_expression0() /
    compound_literal()

rule postfix_expressionT() -> Operation =
    index_operator() /
    "(" _ e:cs0(<node(<assignment_expression0()>)>) _ ")" { Operation::Call(e) } /
    o:node(<member_operator()>) _ i:identifier() { Operation::Member(o, i) } /
    o:node(<postfix_operator()>) { Operation::Unary(o) }

rule index_operator() -> Operation =
    i:node(<index_operator0()>) { Operation::Binary(Node::new(BinaryOperator::Index, i.span), i.node) }

rule index_operator0() -> Node<Expression> =
    "[" _ e:node(<expression0()>) _ "]" { e }

rule member_operator() -> MemberOperator =
    "." { MemberOperator::Direct } /
    "->" { MemberOperator::Indirect }

rule postfix_operator() -> UnaryOperator =
    "++" { UnaryOperator::PostIncrement } /
    "--" { UnaryOperator::PostDecrement }

rule compound_literal() -> Expression =
    n:node(<compound_literal_inner()>) { Expression::CompoundLiteral(Box::new(n)) }

rule compound_literal_inner() -> CompoundLiteral =
    "(" _ t:type_name() _ ")" _ "{" _ i:cs1(<node(<initializer()>)>) _ ","? _ "}" {
        CompoundLiteral {
            type_name: t,
            initializer_list: i,
        }
    }

////
// 6.5.3 Unary operators
////

rule unary_expression() -> Box<Node<Expression>> = box(<node(<unary_expression0()>)>)

rule unary_expression0() -> Expression =
    postfix_expression0() /
    unary_prefix() /
    unary_cast() /
    sizeof_expression() /
    alignof_expression() /
    gnu(<K(<"__extension__">)>) _ e:unary_expression0() { e }

rule unary_prefix() -> Expression =
    n:node(<unary_prefix_inner()>) { Expression::UnaryOperator(Box::new(n)) }

rule unary_prefix_inner() -> UnaryOperatorExpression =
    op:node(<prefix_operator()>) _ e:unary_expression() {
        UnaryOperatorExpression {
            operator: op,
            operand: e,
        }
    }

rule prefix_operator() -> UnaryOperator =
    "++" { UnaryOperator::PreIncrement } /
    "--" { UnaryOperator::PreDecrement } /
    K(<"sizeof">) { UnaryOperator::SizeOf }

rule unary_cast() -> Expression =
    n:node(<unary_cast_inner()>) { Expression::UnaryOperator(Box::new(n)) }

rule unary_cast_inner() -> UnaryOperatorExpression =
    op:node(<unary_operator()>) _ e:cast_expression() {
        UnaryOperatorExpression {
            operator: op,
            operand: e,
        }
    }

rule unary_operator() -> UnaryOperator =
    "&"!"&" { UnaryOperator::Address } /
    "*" { UnaryOperator::Indirection } /
    "+" { UnaryOperator::Plus } /
    "-" { UnaryOperator::Minus } /
    "~" { UnaryOperator::Complement } /
    "!" { UnaryOperator::Negate }

rule sizeof_expression() -> Expression =
    K(<"sizeof">) _ "(" _ t:type_name() _ ")" {
        Expression::SizeOf(Box::new(t))
    }

rule alignof_expression() -> Expression =
    K(<"_Alignof" / gnu(<"__alignof" "__"?>)>) _ "(" _ t:type_name() _ ")" {
        Expression::AlignOf(Box::new(t))
    }

////
// 6.5.4 Cast expressions
////

rule cast_expression() -> Box<Node<Expression>> = box(<node(<cast_expression0()>)>)

rule cast_expression0() -> Expression =
    c:node(<cast_expression_inner()>) { Expression::Cast(Box::new(c)) } /
    unary_expression0()

rule cast_expression_inner() -> CastExpression =
    "(" _ t:type_name() _ ")" _ e:cast_expression() {
        CastExpression {
            type_name: t,
            expression: e,
        }
    }

////
// 6.5.5 -- 6.5.14 Binary operators
////

rule binary_expression() -> Box<Node<Expression>> = box(<binary_expression0()>)

rule binary_expression0() -> Node<Expression> = precedence!{
    // precedence 15 (lowest)
    x:(@) n:node(<"||">)    _ y:@   { infix(n, BinaryOperator::LogicalOr, x, y) }
    --
    // precedence 14
    x:(@) n:node(<"&&">)    _ y:@   { infix(n, BinaryOperator::LogicalAnd, x, y) }
    --
    // precedence 13
    x:(@) n:node(<"|">)     _ y:@   { infix(n, BinaryOperator::BitwiseOr, x, y) }
    --
    // precedence 12
    x:(@) n:node(<"^">)     _ y:@   { infix(n, BinaryOperator::BitwiseXor, x, y) }
    --
    // precedence 11
    x:(@) n:node(<"&"!"&">) _ y:@   { infix(n, BinaryOperator::BitwiseAnd, x, y) }
    --
    // precedence 10
    x:(@) n:node(<"==">)    _ y:@   { infix(n, BinaryOperator::Equals, x, y) }
    x:(@) n:node(<"!=">)    _ y:@   { infix(n, BinaryOperator::NotEquals, x, y) }
    --
    // precedence 9
    x:(@) n:node(<"<">)     _ y:@   { infix(n, BinaryOperator::Less, x, y) }
    x:(@) n:node(<">">)     _ y:@   { infix(n, BinaryOperator::Greater, x, y) }
    x:(@) n:node(<"<=">)    _ y:@   { infix(n, BinaryOperator::LessOrEqual, x, y) }
    x:(@) n:node(<">=">)    _ y:@   { infix(n, BinaryOperator::GreaterOrEqual, x, y) }
    --
    // precedence 7
    x:(@) n:node(<"<<">)    _ y:@   { infix(n, BinaryOperator::ShiftLeft, x, y) }
    x:(@) n:node(<">>">)    _ y:@   { infix(n, BinaryOperator::ShiftRight, x, y) }
    --
    // precedence 6
    x:@   n:node(<"+">)     _ y:(@) { infix(n, BinaryOperator::Plus, x, y) }
    x:@   n:node(<"-">)     _ y:(@) { infix(n, BinaryOperator::Minus, x, y) }
    --
    // precedence 5
    x:(@) n:node(<"*">)     _ y:@   { infix(n, BinaryOperator::Multiply, x, y) }
    x:(@) n:node(<"/">)     _ y:@   { infix(n, BinaryOperator::Divide, x, y) }
    x:(@) n:node(<"%">)     _ y:@   { infix(n, BinaryOperator::Modulo, x, y) }
    --
    n:binary_operand() { n }
    // highest precedence
}

rule infix<T>(ex: rule<T>) -> Node<T> = _ n:node(<ex()>) _ { n }

rule binary_operand() -> Node<Expression> = node(<cast_expression0()>)

////
// 6.5.15 Conditional operator
////

rule conditional_expression() -> Box<Node<Expression>> = box(<node(<conditional_expression0()>)>)

rule conditional_expression0() -> Expression =
    a:binary_expression0() _ t:conditional_expressionT()? {
        if let Some((b, c)) = t {
            let span = Span::span(a.span.start, c.span.end);
            Expression::Conditional(Box::new(Node::new(ConditionalExpression {
                condition: Box::new(a),
                then_expression: b,
                else_expression: c,
            }, span)))
        } else {
            a.node
        }
    }

rule conditional_expressionT() -> (Box<Node<Expression>>, Box<Node<Expression>>) =
    "?" _ a:node(<expression0()>) _ ":" _ b:node(<conditional_expression0()>) { (Box::new(a), Box::new(b)) }

////
// 6.5.16 Assignment operators
////

rule assignment_expression() -> Box<Node<Expression>> = box(<node(<assignment_expression0()>)>)

rule assignment_expression0() -> Expression =
    n:node(<assignment_expression_inner()>) { Expression::BinaryOperator(Box::new(n)) } /
    conditional_expression0()

rule assignment_expression_inner() -> BinaryOperatorExpression =
    a:unary_expression() _ op:node(<assignment_operator()>) _ b:assignment_expression() {
        BinaryOperatorExpression {
            operator: op,
            lhs: a,
            rhs: b,
        }
    }

rule assignment_operator() -> BinaryOperator =
    "=" { BinaryOperator::Assign } /
    "*=" { BinaryOperator::AssignMultiply } /
    "/=" { BinaryOperator::AssignDivide } /
    "%=" { BinaryOperator::AssignModulo } /
    "+=" { BinaryOperator::AssignPlus } /
    "-=" { BinaryOperator::AssignMinus } /
    "<<=" { BinaryOperator::AssignShiftLeft } /
    ">>=" { BinaryOperator::AssignShiftRight } /
    "&=" { BinaryOperator::AssignBitwiseAnd } /
    "^=" { BinaryOperator::AssignBitwiseXor } /
    "|=" { BinaryOperator::AssignBitwiseOr }

////
// 6.5.17 Comma operator
////

pub rule expression() -> Box<Node<Expression>> = box(<node(<expression0()>)>)

rule expression0() -> Expression =
    e:node(<assignment_expression0()>) _ t:list0(<expressionT()>)  {
        if t.len() > 0 {
            let mut t  = t;
            t.insert(0, e);
            Expression::Comma(Box::new(t))
        } else {
            e.node
        }
    }

rule expressionT() -> Node<Expression> =
    "," _ e:node(<assignment_expression0()>) { e }

////
// 6.6 Constant expressions
////

pub rule constant_expression() -> Box<Node<Expression>> = conditional_expression()
rule constant_expression0() -> Expression = conditional_expression0()

////
// 6.7 Declarations
////

pub rule declaration() -> Node<Declaration> = node(<declaration0()>)

rule declaration0() -> Declaration
    = gnu(<K(<"__extension__">)>)? _ d:declaration1() _ ";" {
        Declaration {
            specifiers: d.0,
            declarators: d.1,
        }
    }

rule declaration_seq<H, T>(h: rule<Vec<H>>, t: rule<(Vec<H>, Vec<T>)>) -> (Vec<H>, Vec<T>)
    = h:h() _ t:t() { (concat(h, t.0), t.1) }

rule declaration1() -> (Ds, Id)
    = declaration_seq(<declaration_specifiers_unique()>, <declaration2()>)

rule declaration2() -> (Ds, Id)
    = declaration_seq(<declaration_typedef()>, <declaration_typedef_tail()>)
    / declaration_seq(<declaration_unique_type()>, <declaration_tail(<declaration_specifiers_unique()>)>)
    / declaration_seq(<declaration_typedef()>, <declaration_tail(<declaration_specifiers_nonunique()>)>)

// What can follow a type specifier keyword or typename in a declaration
rule declaration_tail(s: rule<Ds>) -> (Ds, Id)
    = declaration_seq(<s()>, <declaration_tail1(<s()>)>)
rule declaration_tail1(s: rule<(Ds)>) -> (Ds, Id)
    = declaration_seq(<declaration_typedef()>, <declaration_typedef_tail1(<s()>)>)
    / d:declaration_init_declarators() { (Vec::new(), d) }

// What can follow a typedef keyword
rule declaration_typedef_tail() -> (Ds, Id)
    = declaration_seq(<declaration_unique_type()>, <declaration_typedef_tail1(<declaration_specifiers_unique()>)>)
    / declaration_seq(<declaration_nonunique_type()>, <declaration_typedef_tail1(<declaration_specifiers_nonunique()>)>)

// What can follow after typedef + type name
rule declaration_typedef_tail1(s: rule<(Ds)>) -> (Ds, Id)
    = s:s() _ d:declaration_type_declarators() { (s, d) }

rule declaration_unique_type() -> Ds
    = n:node(<declaration_specifier_unique_type0()>) { vec![n] }

rule declaration_nonunique_type() -> Ds
    = n:node(<declaration_specifier_nonunique_type0()>) { vec![n] }

rule declaration_specifiers() -> Ds
    = s:declaration_specifiers_unique() _ t:declaration_specifiers_tail() { concat(s, t) }

rule declaration_specifiers_tail() -> Ds
    = t:declaration_unique_type() _ s:declaration_specifiers_unique() { concat(t, s) }
    / t:declaration_nonunique_type() _ s:declaration_specifiers_nonunique() { concat(t, s) }

rule declaration_specifiers_unique() -> Ds
    = list0(<node(<declaration_specifier_nontype()>)>)

rule declaration_specifiers_nonunique() -> Ds
    = list0(<node(<declaration_specifier_nontype() / declaration_specifier_nonunique_type0()>)>)

rule declaration_specifier_nontype() -> DeclarationSpecifier
    = s:storage_class_specifier() { DeclarationSpecifier::StorageClass(s) }
    / s:type_qualifier() { DeclarationSpecifier::TypeQualifier(s) }
    / s:function_specifier() { DeclarationSpecifier::Function(s) }
    / s:alignment_specifier() { DeclarationSpecifier::Alignment(s) }
    / s:gnu(<attribute_specifier()>) { DeclarationSpecifier::Extension(s) }
    / s:msvc(<msvc_declspec_specifier()>) { DeclarationSpecifier::Extension(s) }
    / s:msvc(<sal_function_annotation()>) { DeclarationSpecifier::Extension(vec![s]) }

rule declaration_typedef() -> Ds
    = s:node(<declaration_typedef0()>) { vec![s] }

rule declaration_typedef0() -> DeclarationSpecifier
    = s:storage_class_typedef() { DeclarationSpecifier::StorageClass(s) }

rule declaration_specifier_unique_type0() -> DeclarationSpecifier
    = s:node(<type_specifier_unique()>) { DeclarationSpecifier::TypeSpecifier(s) }

rule declaration_specifier_nonunique_type0() -> DeclarationSpecifier
    = s:node(<type_specifier_nonunique()>) { DeclarationSpecifier::TypeSpecifier(s) }

rule declaration_init_declarators() -> Id
    = cs0(<node(<init_declarator()>)>)

rule declaration_type_declarators() -> Id
    = cs0(<node(<type_declarator()>)>)

rule init_declarator() -> InitDeclarator =
    d:init_declarator_declarator() _ e:gnu(<init_declarator_gnu()>)? _ i:node(<init_declarator_init()>)?
    {
        InitDeclarator {
            declarator: with_ext(d, e),
            initializer: i,
        }
    }

rule init_declarator_declarator() -> Node<Declarator>
    = d:declarator() {
        env.get().handle_declarator(&d, Symbol::Identifier);
        d
    }

rule init_declarator_init() -> Initializer =
    "=" _ i:initializer() { i }

rule init_declarator_gnu() -> Vec<Node<Extension>> =
    l:asm_label()? _ a:attribute_specifier_list() { l.into_iter().chain(a).collect() }

rule type_declarator() -> InitDeclarator
    = d:declarator() _ e:gnu(<init_declarator_gnu()>)? {
        env.get().handle_declarator(&d, Symbol::Typename);
        InitDeclarator {
            declarator: with_ext(d, e),
            initializer: None,
        }
    }

////
// 6.7.1 Storage-class specifiers
////

rule storage_class_specifier() -> Node<StorageClassSpecifier> = node(<storage_class_specifier0()>)

rule storage_class_specifier0() -> StorageClassSpecifier
    = K(<"extern">) { StorageClassSpecifier::Extern }
    / K(<"static">) { StorageClassSpecifier::Static }
    / K(<"_Thread_local">) { StorageClassSpecifier::ThreadLocal }
    / K(<"auto">) { StorageClassSpecifier::Auto }
    / K(<"register">) { StorageClassSpecifier::Register }

rule storage_class_typedef() -> Node<StorageClassSpecifier>
    = node(<storage_class_typedef0()>)

rule storage_class_typedef0() -> StorageClassSpecifier
    = K(<"typedef">) { StorageClassSpecifier::Typedef }

////
// 6.7.2 Type specifiers
////

// ISO 2011, 6.7.2, §2. Void, _Bool, _Atomic, typedef names, struct/unions, and enum
// specifiers can only appear once in declaration specifiers or specifier-qualifiers.
// This resolves the ambiguity with typedef names.
rule type_specifier_unique() -> TypeSpecifier
    = K(<"void">) { TypeSpecifier::Void }
    / K(<"_Bool">) { TypeSpecifier::Bool }
    / K(<"_Atomic">) _ "(" _ t:type_name() _ ")" { TypeSpecifier::Atomic(t) }
    / s:node(<struct_or_union_specifier()>) { TypeSpecifier::Struct(s) }
    / e:node(<enum_specifier()>) { TypeSpecifier::Enum(e) }
    / t:typedef_name() { TypeSpecifier::TypedefName(t) }

rule type_specifier_nonunique() -> TypeSpecifier
    = K(<"char">) { TypeSpecifier::Char }
    / K(<"short">) { TypeSpecifier::Short }
    / K(<"int">) { TypeSpecifier::Int }
    / K(<"long">) { TypeSpecifier::Long }
    / K(<"float">) { TypeSpecifier::Float }
    / K(<"double">) { TypeSpecifier::Double }
    / K(<"signed" / gnu(<"__signed" "__"?>)>) { TypeSpecifier::Signed }
    / K(<"unsigned">) { TypeSpecifier::Unsigned }
    / K(<"_Complex" / gnu(<"__complex" "__"?>)>) { TypeSpecifier::Complex }
    / t:K(<ts18661_float_type_specifier()>) { TypeSpecifier::TS18661Float(t) }
    / gnu(<typeof_specifier()>)

rule struct_or_union_specifier() -> StructType =
    e:msvc(<list0(<sal_struct_annotation()>)>)?
    _ t:node(<struct_or_union()>) _ i:identifier()? _ d:struct_or_union_body() {
        StructType {
            kind: t,
            identifier: i,
            declarations: d,
            extensions: e.unwrap_or_default(),
        }
    } /
    e:msvc(<list0(<sal_struct_annotation()>)>)?
    _ t:node(<struct_or_union()>) _ i:identifier() {
        StructType {
            kind: t,
            identifier: Some(i),
            declarations: None,
            extensions: e.unwrap_or_default(),
        }
    }

rule struct_or_union_body() -> Option<Vec<Node<StructDeclaration>>> =
    "{" _ d:list1(<node(<struct_declaration()>)>) _ "}" { Some(d) } /
    gnu(<"{" _ "}">) { Some(Vec::new()) } /
    { None }

rule struct_or_union() -> StructKind =
    K(<"struct">) { StructKind::Struct } /
    K(<"union">) { StructKind::Union }

rule struct_declaration() -> StructDeclaration =
    f:node(<struct_field()>) { StructDeclaration::Field(f) } /
    s:static_assert() { StructDeclaration::StaticAssert(s) } /
    gnu(<K(<"__extension__">)>) _ d:struct_declaration() { d }

rule struct_field() -> StructField =
    e:msvc(<list0(<sal_field_annotation()>) >)?
    _ s:specifier_qualifiers()
    _ d:cs0(<node(<struct_declarator()>)>) _ ";" {
        StructField {
            specifiers: s,
            declarators: d,
            extensions: e.unwrap_or_default(),
        }
    }

rule specifier_qualifiers() -> Vec<Node<SpecifierQualifier>>
    = list_eq1_n(<node(<specifier_qualifier_unique_type0()>)>, <node(<specifier_qualifier_qualifier0()>)>)
    / list_ge1_n(<node(<specifier_qualifier_nonunique_type0()>)>, <node(<specifier_qualifier_qualifier0()>)>)

rule specifier_qualifier_unique_type0() -> SpecifierQualifier
    = s:node(<type_specifier_unique()>) { SpecifierQualifier::TypeSpecifier(s) }

rule specifier_qualifier_nonunique_type0() -> SpecifierQualifier
    = s:node(<type_specifier_nonunique()>) { SpecifierQualifier::TypeSpecifier(s) }

rule specifier_qualifier_qualifier0() -> SpecifierQualifier
    = q:type_qualifier() { SpecifierQualifier::TypeQualifier(q) }

rule struct_declarator() -> StructDeclarator =
    d:declarator()? _ ":" _ e:constant_expression() a:gnu(<attribute_specifier_list()>) ? {
        StructDeclarator {
            declarator: d.map(|d| with_ext(d, a)),
            bit_width: Some(e),
        }
    } /
    d:declarator() a:gnu(<attribute_specifier_list()>) ? {
        StructDeclarator {
            declarator: Some(with_ext(d, a)),
            bit_width: None,
        }
    }

rule enum_specifier() -> EnumType =
    K(<"enum">) _ i:identifier()? _ "{" _ e:cs1(<node(<enumerator()>)>) _ ","? _ "}" {
        EnumType {
            identifier: i,
            enumerators: e,
        }
    } /
    K(<"enum">) _ i:identifier() {
        EnumType {
            identifier: Some(i),
            enumerators: Vec::new(),
        }
    }

rule enumerator() -> Enumerator =
    i:identifier() _ e:enumerator_constant()? {
        env.get().add_symbol(&i.node.name, Symbol::Identifier);
        Enumerator {
            identifier: i,
            expression: e,
        }
    }

rule enumerator_constant() -> Box<Node<Expression>> =
    "=" _ e:constant_expression() { e }

////
// 6.7.3 Type qualifiers
////

rule type_qualifier() -> Node<TypeQualifier> = node(<type_qualifier0()>)

rule type_qualifier0() -> TypeQualifier =
    K(<"const"    / gnu(<"__const"()>) >) { TypeQualifier::Const } /
    K(<"restrict" / gnu(<"__restrict" "__"?>)>) { TypeQualifier::Restrict } /
    K(<"volatile" / gnu(<"__volatile" "__"?>)>) { TypeQualifier::Volatile } /
    clang(<K(<"_Nonnull">)>) { TypeQualifier::Nonnull } /
    clang(<K(<"_Null_unspecified">)>) { TypeQualifier::NullUnspecified } /
    clang(<K(<"_Nullable">)>) { TypeQualifier::Nullable } /
    // 6.7.2.4: _Atomics followed by a "(" are interpreted as type specifiers.
    K(<"_Atomic">) _ !"(" { TypeQualifier::Atomic } /
    c:msvc(<calling_convention()>)  { TypeQualifier::CallingConvention(c) }

////
// 6.7.4 Function specifiers
////

rule function_specifier() -> Node<FunctionSpecifier> = node(<function_specifier0()>)

rule function_specifier0() -> FunctionSpecifier =
    K(<"inline" / gnu(<"__inline" "__"?>) / msvc(<"__inline">)>) { FunctionSpecifier::Inline } /
    K(<msvc(<"__forceinline"()>) >) { FunctionSpecifier::ForceInline } /
    K(<"_Noreturn">) { FunctionSpecifier::Noreturn }

////
// 6.7.5 Alignment specifiers
////

rule alignment_specifier() -> Node<AlignmentSpecifier> = node(<alignment_specifier0()>)

rule alignment_specifier0() -> AlignmentSpecifier =
    K(<"_Alignas">) _ "(" _ t:type_name() _ ")" { AlignmentSpecifier::Type(t) } /
    K(<"_Alignas">) _ "(" _ e:constant_expression() _ ")" { AlignmentSpecifier::Constant(e) } /
    msvc(<K(<"__unaligned">)>) { AlignmentSpecifier::Unaligned }

////
// 6.7.6 Declarators
////

rule declarator() -> Node<Declarator> = node(<declarator0()>)

rule declarator0() -> Declarator =
    attr:gnu(<attribute_specifier_list()>) ?
    pointer:list0(<pointer()>)  _
    kind:node(<direct_declarator()>) _
    derived:list0(<node(<derived_declarator()>)>)
    {
        Declarator {
            kind: kind,
            derived: concat(pointer, derived),
            extensions: attr.unwrap_or_default(),
        }
    }

rule direct_declarator() -> DeclaratorKind =
    i:identifier() { DeclaratorKind::Identifier(i) } /
    "(" _ d:declarator() _ ")" { DeclaratorKind::Declarator(Box::new(d)) }

rule derived_declarator() -> DerivedDeclarator =
    "[" _ a:node(<array_declarator()>) { DerivedDeclarator::Array(a) } /
    "(" _ f:node(<function_declarator()>) _ ")" { DerivedDeclarator::Function(f) } /
    "(" _ p:cs0(<identifier()>) _ ")" { DerivedDeclarator::KRFunction(p) }

rule array_declarator() -> ArrayDeclarator =
    q:list0(<type_qualifier()>) _ "]" {
        ArrayDeclarator {
            qualifiers: q,
            size: ArraySize::Unknown,
        }
    } /
    q:list0(<type_qualifier()>) _ e:assignment_expression() _ "]" {
        ArrayDeclarator {
            qualifiers: q,
            size: ArraySize::VariableExpression(e),
        }
    } /
    K(<"static">) _ q:list0(<type_qualifier()>) _ e:assignment_expression() _ "]" {
        ArrayDeclarator {
            qualifiers: q,
            size: ArraySize::StaticExpression(e),
        }
    } /
    q:list1(<type_qualifier()>) _ K(<"static">) _ e:assignment_expression() _ "]" {
        ArrayDeclarator {
            qualifiers: q,
            size: ArraySize::StaticExpression(e),
        }
    } /
    q:list0(<type_qualifier()>) _ "*" _ "]" {
        ArrayDeclarator {
            qualifiers: q,
            size: ArraySize::VariableUnknown,
        }
    }

rule function_declarator() -> FunctionDeclarator =
    p:cs1(<parameter_declaration()>) _ e:ellipsis() {
        FunctionDeclarator {
            parameters: p,
            ellipsis: e,
        }
    }

rule pointer() -> Node<DerivedDeclarator> = node(<pointer0()>)

rule pointer0() -> DerivedDeclarator =
    "*" _ q:list0(<node(<pointer_qualifier()>)>) { DerivedDeclarator::Pointer(q) }

rule pointer_qualifier() -> PointerQualifier =
    q:type_qualifier() { PointerQualifier::TypeQualifier(q) } /
    e:gnu(<attribute_specifier()>)  { PointerQualifier::Extension(e) }

rule ellipsis() -> Ellipsis =
    "," _ "..." { Ellipsis::Some } / { Ellipsis::None }

rule parameter_declaration() -> Node<ParameterDeclaration> = node(<parameter_declaration0()>)

rule parameter_declaration0() -> ParameterDeclaration =
    b:msvc(<sal_param_annotation()>) ?
    _ s:declaration_specifiers()
    _ d:parameter_declarator()     _ a:gnu(<attribute_specifier_list()>) ? {
        ParameterDeclaration {
            specifiers: s,
            declarator: d,
            extensions: a.unwrap_or_else(|| b.map(|x| vec![x]).unwrap_or_else(|| vec![]))
        }
    }

rule parameter_declarator() -> Option<Node<Declarator>> =
    d:declarator() { Some(d) } /
    d:abstract_declarator() { Some(d) } /
    { None }

////
// 6.7.7 Type names
////

rule type_name() -> Node<TypeName> = node(<type_name0()>)

rule type_name0() -> TypeName =
    s:specifier_qualifiers()  _ d:abstract_declarator()? {
        TypeName {
            specifiers: s,
            declarator: d,
        }
    }

rule abstract_declarator() -> Node<Declarator> = node(<abstract_declarator0()>)

rule abstract_declarator0() -> Declarator =
    p:list0(<pointer()>)  _ k:node(<direct_abstract_declarator()>) _ d:list0(<derived_abstract_declarator()>)  {
        Declarator {
            kind: k,
            derived: concat(p, d),
            extensions: Vec::new(),
        }
    } /
    p:list0(<pointer()>)  k:position!() _ d:list1(<derived_abstract_declarator()>)  {
        Declarator {
            kind: Node::new(DeclaratorKind::Abstract, Span::span(k, k)),
            derived: concat(p, d),
            extensions: Vec::new(),
        }
    } /
    p:list1(<pointer()>)  k:position!() {
        Declarator {
            kind: Node::new(DeclaratorKind::Abstract, Span::span(k, k)),
            derived: p,
            extensions: Vec::new(),
        }
    }

rule direct_abstract_declarator() -> DeclaratorKind =
    "(" _ d:abstract_declarator() _ ")" { DeclaratorKind::Declarator(Box::new(d)) }

rule derived_abstract_declarator() -> Node<DerivedDeclarator> = node(<derived_abstract_declarator0()>)

rule derived_abstract_declarator0() -> DerivedDeclarator =
    "[" _ a:node(<abstract_array_declarator()>) { DerivedDeclarator::Array(a) } /
    "(" _ d:node(<abstract_function_declarator()>) _ ")" { DerivedDeclarator::Function(d) }

rule abstract_array_declarator() -> ArrayDeclarator =
    q:list0(<type_qualifier()>)  _ "]" {
        ArrayDeclarator {
            qualifiers: q,
            size: ArraySize::Unknown,
        }
    } /
    q:list0(<type_qualifier()>)  _ e:assignment_expression() _ "]" {
        ArrayDeclarator {
            qualifiers: q,
            size: ArraySize::VariableExpression(e),
        }
    } /
    K(<"static">) _ q:list0(<type_qualifier()>)  _ e:assignment_expression() _ "]" {
        ArrayDeclarator {
            qualifiers: q,
            size: ArraySize::StaticExpression(e),
        }
    } /
    q:list1(<type_qualifier()>)  _ K(<"static">) _ e:assignment_expression() _ "]" {
        ArrayDeclarator {
            qualifiers: q,
            size: ArraySize::StaticExpression(e),
        }
    } /
    "*" _ "]" {
        ArrayDeclarator {
            qualifiers: Vec::new(),
            size: ArraySize::VariableUnknown,
        }
    }

rule abstract_function_declarator() -> FunctionDeclarator =
    p:cs1(<parameter_declaration()>)  _ e:ellipsis() {
        FunctionDeclarator {
            parameters: p,
            ellipsis: e,
        }
    } /
    {
        FunctionDeclarator {
            parameters: Vec::new(),
            ellipsis: Ellipsis::None,
        }
    }


////
// 6.7.8 Type definitions
////

rule typedef_name() -> Node<Identifier> = quiet!{ typedef_name0() / expected!("<typedef_name>") }

rule typedef_name0() -> Node<Identifier> = i:identifier() {?
    if env.get().is_typename(&i.node.name) {
        Ok(i)
    } else {
        Err("<unused>")
    }
}

////
// 6.7.9 Initialization
////

rule initializer() -> Initializer =
    e:assignment_expression() { Initializer::Expression(e) } /
    "{" _ i:cs1(<node(<initializer_list_item()>)>) _ ","? _ "}" { Initializer::List(i) } /
    gnu(<"{" _ "}">) { Initializer::List(Vec::new()) }

rule initializer_list_item() -> InitializerListItem =
    d:designation()? _ i:node(<initializer()>) {
        InitializerListItem {
            designation: d.unwrap_or_default(),
            initializer: Box::new(i),
        }
    }

rule designation() -> Vec<Node<Designator>> =
    d:list1(<node(<designator()>)>) _ "=" { d } /
    d:gnu(<node(<colon_designation()>)>) { vec! [ d ] } /
    d:gnu(<node(<array_designator()>)>) { vec![ d ] }

rule colon_designation() -> Designator =
    i:identifier() _ ":" { Designator::Member(i) }

rule designator() -> Designator =
    d:array_designator() { d } /
    "." _ i:identifier() { Designator::Member(i) }

rule array_designator() -> Designator =
    "[" _ a:node(<constant_expression0()>) _ b:gnu(<range_designator_ext()>) ? "]" {
        match b {
            Some(b) => {
                let span = Span::span(a.span.start, b.span.end);
                Designator::Range(Node::new(RangeDesignator { from: a, to: b }, span))
            }
            None => Designator::Index(a),
        }
    }

rule range_designator_ext() -> Node<Expression> =
    "..." _ e:node(<constant_expression0()>) { e }

////
// 6.7.10 Static assertions
////

rule static_assert() -> Node<StaticAssert> = node(<static_assert0()>)

rule static_assert0() -> StaticAssert =
   gnu(<K(<"__extension__">)>)?
   _ K(<"_Static_assert">) _ "(" _ e:constant_expression() _ "," _ s:string_literal() _ ")" _ ";" {
        StaticAssert {
            expression: e,
            message: s,
        }
    }

////
// 6.8 Statements and blocks
////

pub rule statement() -> Box<Node<Statement>> = box(<node(<statement0()>)>)

rule statement0() -> Statement =
    s:node(<labeled_statement()>) { Statement::Labeled(s) } /
    compound_statement() /
    expression_statement() /
    selection_statement() /
    iteration_statement() /
    jump_statement() // /
    // gnu(<asm_statement()>)

////
// 6.8.1 Labeled statements
////

rule labeled_statement() -> LabeledStatement =
    l:node(<label()>) _ ":" _ s:statement() {
        LabeledStatement {
            label: l,
            statement: s,
        }
    }

rule label() -> Label =
    i:identifier() { Label::Identifier(i) } /
    K(<"case">) _ e:constant_expression() { Label::Case(e) } /
    K(<"default">) { Label::Default }

////
// 6.8.2 Compound statement
////

rule compound_statement() -> Statement =
    "{" _ b:list0(<node(<block_item()>)>) _ "}" { Statement::Compound(b) }

rule block_item() -> BlockItem =
    d:declaration() { BlockItem::Declaration(d) } /
    s:static_assert() { BlockItem::StaticAssert(s) } /
    s:node(<statement0()>) { BlockItem::Statement(s) }

////
// 6.8.3 Expression and null statements
////

rule expression_statement() -> Statement =
    e:expression()? _ ";" { Statement::Expression(e) }

////
// 6.8.4 Selection statement
////

rule selection_statement() -> Statement =
    s:node(<if_statement()>) { Statement::If(s) } /
    s:node(<switch_statement()>) { Statement::Switch(s) }

rule if_statement() -> IfStatement =
    K(<"if">) _ "(" _ e:expression() _ ")" _ a:statement() _ b:else_statement()? {
        IfStatement {
            condition: e,
            then_statement: a,
            else_statement: b,
        }
    }

rule else_statement() -> Box<Node<Statement>> = K(<"else">) _ s:statement() { s }

rule switch_statement() -> SwitchStatement =
    K(<"switch">) _ "(" _ e:expression() _ ")" _ s:statement() {
        SwitchStatement {
            expression: e,
            statement: s,
        }
    }

////
// 6.8.5 Iteration statement
////

rule iteration_statement() -> Statement =
    s:node(<while_statement()>) { Statement::While(s) } /
    s:node(<do_while_statement()>) { Statement::DoWhile(s) } /
    s:node(<for_statement()>) { Statement::For(s) }

rule while_statement() -> WhileStatement =
    K(<"while">) _ "(" _ e:expression() _ ")" _ s:statement() {
        WhileStatement {
            expression: e,
            statement: s,
        }
    }

rule do_while_statement() -> DoWhileStatement =
    K(<"do">) _ s:statement() _ K(<"while">) _ "(" _ e:expression() _ ")" _ ";" {
        DoWhileStatement {
            statement: s,
            expression: e,
        }
    }

rule for_statement() -> ForStatement =
    K(<"for">) _ "(" _ a:node(<for_initializer()>) _ b:expression()? _ ";" _ c:expression()? _ ")" _ s:statement() {
        ForStatement {
            initializer: a,
            condition: b,
            step: c,
            statement: s,
        }
    }

rule for_initializer() -> ForInitializer =
    e:expression() _ ";" { ForInitializer::Expression(e) } /
    d:declaration() { ForInitializer::Declaration(d) } /
    s:static_assert() { ForInitializer::StaticAssert(s) } /
    ";" { ForInitializer::Empty }

////
// 6.8.6 Jump statements
////

rule jump_statement() -> Statement =
    K(<"goto">) _ i:identifier() _ ";" { Statement::Goto(i) } /
    K(<"continue">) _ ";" { Statement::Continue } /
    K(<"break">) _ ";" { Statement::Break } /
    K(<"return">) _ e:expression()? _ ";" { Statement::Return(e) }

////
// 6.9 External definitions
////

rule scoped<T>(e: rule<T>) -> T = ({ env.get().enter_scope(); }) e:e()? {? env.get().leave_scope(); e.ok_or("") }

rule traced<T>(e: rule<T>) -> T =
    &(input:$([_]*) {
        #[cfg(feature = "trace")]
        println!("[PEG_INPUT_START]\n{}\n[PEG_TRACE_START]", input);
    })
    e:e()? {?
        #[cfg(feature = "trace")]
        println!("[PEG_TRACE_STOP]");
        e.ok_or("")
    }

pub rule translation_unit() -> TranslationUnit = traced(<translation_unit0()>)

rule translation_unit0() -> TranslationUnit =
    d:list0(<node(<external_declaration()>)>) _ { TranslationUnit(d) }

rule external_declaration() -> ExternalDeclaration =
    d:declaration() { ExternalDeclaration::Declaration(d) } /
    s:static_assert() { ExternalDeclaration::StaticAssert(s) } /
    d:scoped(<node(<function_definition()>)>) { ExternalDeclaration::FunctionDefinition(d) }

rule function_definition() -> FunctionDefinition
    = gnu(<K(<"__extension__">)>)?
    _ a:declaration_specifiers() _ b:declarator() _ c:list0(<declaration()>)
    _ d:node(<compound_statement()>) {
        FunctionDefinition {
            specifiers: a,
            declarator: b,
            declarations: c,
            statement: d,
        }
    }

////
// GNU extensions
////

rule gnu<E>(e: rule<E>) -> E = &gnu_guard() e:e() { e }

rule gnu_guard() = {? if env.get().extensions_gnu { Ok(()) } else { Err("gnu extensions disabled") } }

rule msvc_declspec_specifier() -> Vec<Node<Extension>> =
    K(<"__declspec">) _ "(" _ a:cs0(<node(<msvc_attribute()>)>) _ ")" { a }

rule msvc_attribute() -> Extension =
    n:node(<attribute_name()>) _ p:attribute_parameters()? {
        Extension::Attribute(Attribute {
            name: n,
            arguments: p.unwrap_or_default(),
        })
    }

////
// GNU attributes
////

rule attribute_specifier_list() -> Vec<Node<Extension>> =
    a:list0(<attribute_specifier()>) { a.into_iter().flat_map(|v| v).collect() }

rule attribute_specifier() -> Vec<Node<Extension>> =
    K(<"__attribute__">) _ "((" _ a:cs0(<node(<attribute()>)>) _ "))" { a }

rule attribute() -> Extension =
    c:clang(<node(<attr_availability()>)>) { Extension::AvailabilityAttribute(c) } /
    n:node(<attribute_name()>) _ p:attribute_parameters()? {
        Extension::Attribute(Attribute {
            name: n,
            arguments: p.unwrap_or_default(),
        })
    }

rule attribute_name() -> String =
    n:$(quiet!{ ['_' | 'a'..='z' | 'A'..='Z']['_' | 'a'..='z' | 'A'..='Z' | '0'..='9']* }) { String::from(n) }

rule attribute_parameters() -> Vec<Node<Expression>> =
    "(" _ e:cs0(<node(<assignment_expression0()>)>) _ ")" { e }

rule attr_availability() -> AvailabilityAttribute =
    K(<"availability">) _ "(" _ p:identifier() _ "," _ c:cs1(<node(<attr_availability_clause()>)>) _ ")" {
        AvailabilityAttribute {
            platform: p,
            clauses: c,
        }
    }

rule attr_availability_clause() -> AvailabilityClause =
    K(<"introduced">) _ "=" _ v:node(<attr_availability_version()>) { AvailabilityClause::Introduced(v) } /
    K(<"deprecated">) _ "=" _ v:node(<attr_availability_version()>) { AvailabilityClause::Deprecated(v) } /
    K(<"obsoleted">) _ "=" _ v:node(<attr_availability_version()>) { AvailabilityClause::Obsoleted(v) } /
    K(<"unavailable">) { AvailabilityClause::Unavailable } /
    K(<"message">) _ "=" _ s:string_literal() { AvailabilityClause::Message(s) } /
    K(<"replacement">) _ "=" _ s:string_literal() { AvailabilityClause::Replacement(s) }

rule attr_availability_version() -> AvailabilityVersion =
    a:$(dec()+) b:$("." $(dec()+) )? c:$("." $(dec()+) )? {
        AvailabilityVersion {
            major: a.into(),
            minor: b.map(str::to_owned),
            subminor: c.map(str::to_owned),
        }
    }

////
// GNU assembler labels
////

rule asm_label() -> Node<Extension> = node(<asm_label0()>)

rule asm_label0() -> Extension =
    asm_label_keyword() _ "(" _ s:string_literal() _ ")" { Extension::AsmLabel(s) }

rule asm_label_keyword() =
    quiet!{K(<"asm">) / K(<"__asm" "__"?>)} / expected!("asm")

////
// GNU assembler statements
////

// rule asm_statement() -> Statement =
//     s:node(<asm_statement0()>) { Statement::Asm(s) }

// rule asm_statement0() -> AsmStatement =
//     K(<"asm" / "__asm" "__"?>) _ q:type_qualifier()? _ "(" _
//         a:string_literal() _
//         o:asm_ext(<asm_operand_list()>,
//             <asm_ext(<asm_operand_list()>,
//                 <asm_ext(
//                     <cs0(<string_literal()>)>,
//                     <()>
//                 )>
//             )>)? _
//     ")" _ ";" {
//         if let Some((o, (i, (c, ())))) = o {
//             AsmStatement::GnuExtended(GnuExtendedAsmStatement {
//                 qualifier: q,
//                 template: a,
//                 outputs: o,
//                 inputs: i,
//                 clobbers: c,
//             })
//         } else {
//             AsmStatement::GnuBasic(a)
//         }
//     }

// rule asm_ext<E, T>(e: rule<E>, t: rule<T>) -> (E, T) = ":" _ e:e() _ t:t()? { (e, t.unwrap_or_default()) }

// rule asm_operand_list() -> Vec<Node<GnuAsmOperand>> = cs0(<node(<asm_operand()>)>)

// rule asm_operand() -> GnuAsmOperand =
//     i:("[" _ i:identifier() _ "]" _ {i})? s:string_literal() _ "(" _ e:node(<expression0()>) _ ")" {
//         GnuAsmOperand {
//             symbolic_name: i,
//             constraints: s,
//             variable_name: e,
//         }
//     }

////
// GNU expression extensions
////

rule gnu_primary_expression() -> Expression =
    statement_expression() /
    offsetof_expression() /
    va_arg_expression() /
    keyword_expression()

rule statement_expression() -> Expression =
    "(" _ s:scoped(<node(<compound_statement()>)>) _ ")" { Expression::Statement(Box::new(s)) }

rule va_arg_expression() -> Expression =
    n:node(<va_arg_expression_inner()>) { Expression::VaArg(Box::new(n)) }

rule va_arg_expression_inner() -> VaArgExpression =
    K(<"__builtin_va_arg">) _ "(" _ e:assignment_expression() _ "," _ t:type_name() _ ")" {
        VaArgExpression {
            va_list: e,
            type_name: t,
        }
    }

rule keyword_expression() -> Expression =
    k:node(<$(keyword_expression0())>) {
        let ident = Identifier {
            name: k.node.to_string(),
        };
        Expression::Identifier(Box::new(Node::new(ident, k.span)))
    }

rule keyword_expression0() =
    K(<"__func__">) /
    K(<"__FUNCTION__">) /
    K(<"__PRETTY_FUNCTION__">)

rule offsetof_expression() -> Expression =
    n:node(<offsetof_expression_inner()>) { Expression::OffsetOf(Box::new(n)) }

rule offsetof_expression_inner() -> OffsetOfExpression =
    K(<"__builtin_offsetof">) _ "(" _ t:type_name() _ "," _ d:node(<offsetof_designator()>) _ ")" {
        OffsetOfExpression {
            type_name: t,
            designator: d,
        }
    }

rule offsetof_designator() -> OffsetDesignator =
    i:identifier() _ d:list0(<node(<offsetof_member()>)>) {
        OffsetDesignator {
            base: i,
            members: d,
        }
    }

rule offsetof_member() -> OffsetMember =
    "." _ i:identifier() { OffsetMember::Member(i) } /
    "->" _ i:identifier() { OffsetMember::IndirectMember(i) } /
    "[" _ e:node(<expression0()>) _ "]" { OffsetMember::Index(e) }

////
// GNU typeof extension
////

rule typeof_specifier() -> TypeSpecifier =
    K(<"typeof" / "__typeof" "__"?>) _ "(" _ e:node(<typeof_specifier0()>) _ ")" { TypeSpecifier::TypeOf(e) }

rule typeof_specifier0() -> TypeOf =
    e:node(<expression0()>) { TypeOf::Expression(e) } /
    t:type_name() { TypeOf::Type(t) }

////
// ISO/IEC TS 18661 series floating point extensions
////

rule ts18661_float_type_specifier() -> TS18661FloatType =
    ts18661_binary_float() /
    ts18661_decimal_float()

rule ts18661_binary_float() -> TS18661FloatType =
    "_Float" width:ts18661_binary_width() extended:"x"? {
        ts18661_float(true, width, extended.is_some())
    }

rule ts18661_binary_width() -> usize =
    n:$("16" / "32" / "64" / "128") {
        n.parse().unwrap()
    }

rule ts18661_decimal_float() -> TS18661FloatType =
    "_Decimal" width:ts18661_decimal_width() extended:"x"? {
        ts18661_float(false, width, extended.is_some())
    }

rule ts18661_decimal_width() -> usize =
    n:$("32" / "64" / "128") {
        n.parse().unwrap()
    }

rule ts18661_float_suffix() -> TS18661FloatType =
    ("df" / "DF") { ts18661_float(false, 32, false) } /
    ("dd" / "DD") { ts18661_float(false, 64, false) } /
    ("dl" / "DL") { ts18661_float(false, 128, false) } /

    ['f'|'F'] width:ts18661_binary_width() extended:"x"? {
        ts18661_float(true, width, extended.is_some())
    } /
    ['d'|'D'] width:ts18661_decimal_width() extended:"x"? {
        ts18661_float(false, width, extended.is_some())
    }

////
// Clang extensions
////

rule clang<E>(e: rule<E>) -> E = &clang_guard() e:e() { e }

rule clang_guard() = {? if env.get().extensions_clang { Ok(()) } else { Err("clang extensions disabled") } }

////
// MSVC extensions
////

rule msvc<E>(e: rule<E>) -> E = &msvc_guard() e:e() { e }

rule msvc_guard() = {? if env.get().extensions_msvc { Ok(()) } else { Err("msvc extensions disabled") } }

////
// MSVC source-code annotation language (SAL) extensions
////

rule sal_param_annotation0() -> Extension =
    K(<"_In_">) { Extension::SalParamAttribute(SalParamAttribute::In) } /
    K(<"_Out_">) { Extension::SalParamAttribute(SalParamAttribute::Out) } /
    K(<"_Inout_">) { Extension::SalParamAttribute(SalParamAttribute::InOut) } /
    K(<"_Outptr_">) { Extension::SalParamAttribute(SalParamAttribute::OutPtr) } /
    K(<"_Outptr_result_maybenull_">) { Extension::SalParamAttribute(SalParamAttribute::OutPtrResultMaybeNull) } /
    K(<"_Outptr_result_bytebuffer_">) _ "(" _ p:sal_expression() _ ")" { Extension::SalParamAttribute(SalParamAttribute::OutPtrResultBytebuffer(p)) } /
    K(<"_In_opt_">) { Extension::SalParamAttribute(SalParamAttribute::InOpt) } /
    K(<"_Out_opt_">) { Extension::SalParamAttribute(SalParamAttribute::OutOpt) } /
    K(<"_Inout_opt_">) { Extension::SalParamAttribute(SalParamAttribute::InOutOpt) } /
    K(<"_Outptr_opt_">) { Extension::SalParamAttribute(SalParamAttribute::OutPtrOpt) } /
    K(<"_In_reads_">) _ "(" _ p:sal_expression() _ ")" { Extension::SalParamAttribute(SalParamAttribute::InReads(p)) } /
    K(<"_In_reads_opt_">) _ "(" _ p:sal_expression() _ ")" { Extension::SalParamAttribute(SalParamAttribute::InReadsOpt(p)) } /
    K(<"_In_reads_bytes_">) _ "(" _ p:sal_expression() _ ")" { Extension::SalParamAttribute(SalParamAttribute::InReadsBytes(p)) } /
    K(<"_In_reads_bytes_opt_">) _ "(" _ p:sal_expression() _ ")" { Extension::SalParamAttribute(SalParamAttribute::InReadsBytesOpt(p)) } /
    K(<"_Out_writes_">) _ "(" _ p:sal_expression() _ ")" { Extension::SalParamAttribute(SalParamAttribute::OutWrites(p)) } /
    K(<"_Out_writes_opt_">) _ "(" _ p:sal_expression() _ ")" { Extension::SalParamAttribute(SalParamAttribute::OutWritesOpt(p)) } /
    K(<"_Out_writes_bytes_">) _ "(" _ p:sal_expression() _ ")" { Extension::SalParamAttribute(SalParamAttribute::OutWritesBytes(p)) } /
    K(<"_Out_writes_bytes_opt_">) _ "(" _ p:sal_expression() _ ")" { Extension::SalParamAttribute(SalParamAttribute::OutWritesBytesOpt(p)) } /
    K(<"_Inout_updates_">) _ "(" _ p:sal_expression() _ ")" { Extension::SalParamAttribute(SalParamAttribute::InOutUpdates(p)) } /
    K(<"_Inout_updates_opt_">) _ "(" _ p:sal_expression() _ ")" { Extension::SalParamAttribute(SalParamAttribute::InOutUpdatesOpt(p)) } /
    K(<"_Inout_updates_bytes_">) _ "(" _ p:sal_expression() _ ")" { Extension::SalParamAttribute(SalParamAttribute::InOutUpdatesBytes(p)) } /
    K(<"_Inout_updates_bytes_opt_">) _ "(" _ p:sal_expression() _ ")" { Extension::SalParamAttribute(SalParamAttribute::InOutUpdatesBytesOpt(p)) } /
    K(<"_Out_writes_to_">) _ "(" _ s:sal_primary_expression() _ "," _ c:sal_primary_expression() _ ")" {
        Extension::SalParamAttribute(SalParamAttribute::OutWritesTo(s, c))
    } /
    K(<"_Out_writes_bytes_to_">) _ "(" _ s:sal_primary_expression() _ "," _ c:sal_primary_expression() _ ")" {
        Extension::SalParamAttribute(SalParamAttribute::OutWritesBytesTo(s, c))
    } /
    K(<"_Reserved_">) { Extension::SalParamAttribute(SalParamAttribute::Reserved) }

rule sal_param_annotation() -> Node<Extension> = node(<sal_param_annotation0()>)

rule sal_function_annotation0() -> Extension =
    K(<"_Success_">) _ "(" _ e:sal_expression() _ ")" { Extension::SalFunctionAttribute(SalFunctionAttribute::Success(e)) } /
    K(<"_Return_type_success_">) _ "(" _ e:sal_expression() _ ")" { Extension::SalFunctionAttribute(SalFunctionAttribute::ReturnTypeSuccess(e)) } /
    K(<"_Null_terminated_">) { Extension::SalFunctionAttribute(SalFunctionAttribute::NullTerminated) } /
    K(<"_NullNull_terminated_">) { Extension::SalFunctionAttribute(SalFunctionAttribute::NullNullTerminated) } /
    K(<"_Check_return_">) { Extension::SalFunctionAttribute(SalFunctionAttribute::CheckReturn) } /
    K(<"_Must_inspect_result_">) { Extension::SalFunctionAttribute(SalFunctionAttribute::MustInspectResult) } /
    K(<"_Use_decl_annotations_">) { Extension::SalFunctionAttribute(SalFunctionAttribute::UseDeclAnnotations) } /
    K(<"_Maybe_raises_SEH_exception_">) { Extension::SalFunctionAttribute(SalFunctionAttribute::MaybeRaisesSehException) } /
    K(<"_Raises_SEH_exception_">) { Extension::SalFunctionAttribute(SalFunctionAttribute::RaisesSehException) } /
    K(<"_When_">) _ "(" _ e:$([_]+) _ ")" { Extension::SalFunctionAttribute(SalFunctionAttribute::When(e.into())) }
rule sal_function_annotation() -> Node<Extension> = node(<sal_function_annotation0()>)

rule sal_field_annotation0() -> Extension =
    K(<"_Field_z_">) { Extension::SalFieldAttribute(SalFieldAttribute::FieldZ) } /
    K(<"_Field_range_">) _ "(" _ s:sal_primary_expression() _ "," _ c:sal_primary_expression() _ ")" {
        Extension::SalFieldAttribute(SalFieldAttribute::FieldRange(s, c))
    } /
    K(<"_Satisfies_">) _ "(" _ e:sal_expression() _ ")" {
        Extension::SalFieldAttribute(SalFieldAttribute::Satisfies(e))
    } /
    K(<"_Field_size_">) _ "(" _ e:sal_expression() _ ")" {
        Extension::SalFieldAttribute(SalFieldAttribute::FieldSize(e))
    } /
    K(<"_Field_size_opt_">) _ "(" _ e:sal_expression() _ ")" {
        Extension::SalFieldAttribute(SalFieldAttribute::FieldSizeOpt(e))
    } /
    K(<"_Field_size_bytes_">) _ "(" _ e:sal_expression() _ ")" {
        Extension::SalFieldAttribute(SalFieldAttribute::FieldSizeBytes(e))
    } /
    K(<"_Field_size_bytes_opt_">) _ "(" _ e:sal_expression() _ ")" {
        Extension::SalFieldAttribute(SalFieldAttribute::FieldSizeBytesOpt(e))
    } /
    K(<"_Field_size_part_">) _ "(" _ s:sal_primary_expression() _ "," _ c:sal_primary_expression() _ ")" {
        Extension::SalFieldAttribute(SalFieldAttribute::FieldSizePart(s, c))
    } /
    K(<"_Field_size_part_opt_">)_ "(" _ s:sal_primary_expression() _ "," _ c:sal_primary_expression() _ ")" {
        Extension::SalFieldAttribute(SalFieldAttribute::FieldSizePartOpt(s, c))
    } /
    K(<"_Field_size_bytes_part_">) _ "(" _ s:sal_primary_expression() _ "," _ c:sal_primary_expression() _ ")" {
        Extension::SalFieldAttribute(SalFieldAttribute::FieldSizeBytesPart(s, c))
    } /
    K(<"_Field_size_bytes_part_opt_">) _ "(" _ s:sal_primary_expression() _ "," _ c:sal_primary_expression() _ ")" {
        Extension::SalFieldAttribute(SalFieldAttribute::FieldSizeBytesPartOpt(s, c))
    } /
    K(<"_Field_size_full_">) _ "(" _ e:sal_expression() _ ")" {
        Extension::SalFieldAttribute(SalFieldAttribute::FieldSizeFull(e))
    } /
    K(<"_Field_size_full_opt_">) _ "(" _ e:sal_expression() _ ")" {
        Extension::SalFieldAttribute(SalFieldAttribute::FieldSizeFullOpt(e))
    } /
    K(<"_Field_size_bytes_full_">) _ "(" _ e:sal_expression() _ ")" {
        Extension::SalFieldAttribute(SalFieldAttribute::FieldSizeBytesFull(e))
    } /
    K(<"_Field_size_bytes_full_opt_">) _ "(" _ e:sal_expression() _ ")" {
        Extension::SalFieldAttribute(SalFieldAttribute::FieldSizeBytesFullOpt(e))
    }
rule sal_field_annotation() -> Node<Extension> = node(<sal_field_annotation0()>)

rule sal_struct_annotation0() -> Extension =
    K(<"_Struct_size_bytes_">) _ "(" _ e:sal_expression() _ ")" {
        Extension::SalStructAttribute(SalStructAttribute::StructSizeBytes(e))
    }
rule sal_struct_annotation() -> Node<Extension> = node(<sal_struct_annotation0()>)

rule calling_convention() -> CallingConvention =
    K(<"__cdecl">) { CallingConvention::Cdecl } /
    K(<"__stdcall">) { CallingConvention::Stdcall }

rule sal_ignore_reserved<T>(e: rule<T>) -> T = ({ env.get().ignore_reserved(true); }) e:e() {? env.get().ignore_reserved(false); Ok(e) }
rule sal_expression() -> Node<Expression> = e:sal_ignore_reserved(<expression()>) { *e }
rule sal_primary_expression() -> Node<Expression> = e:sal_ignore_reserved(<primary_expression()>) { *e }

}}
