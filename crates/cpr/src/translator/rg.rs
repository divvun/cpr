use lang_c::ast;
use once_cell::sync::Lazy;
use std::{
    collections::HashSet,
    fmt::{self, Write},
};

static RUST_KEYWORDS: Lazy<HashSet<String>> = Lazy::new(|| {
    let mut set = HashSet::new();
    let source = "as break const continue crate else enum extern false fn for 
    if impl in let loop match mod move mut pub ref return self Self static struct 
    super trait true type unsafe use where while";
    for kw in source
        .replace("\n", " ")
        .split(" ")
        .map(|x| x.trim())
        .filter(|x| !x.is_empty())
    {
        set.insert(kw.to_string());
    }
    set
});

pub const INDENT: &str = "    "; // 4 spaces

pub struct IndentedWriter<'a> {
    w: &'a mut dyn fmt::Write,
    state: IndentState,
}

#[derive(Debug, PartialEq)]
enum IndentState {
    Pending,
    Emitted,
}

impl<'a> IndentedWriter<'a> {
    fn emit_indent(&mut self) -> fmt::Result {
        self.w.write_str(INDENT)?;
        self.transition(IndentState::Pending, IndentState::Emitted);
        Ok(())
    }

    fn transition(&mut self, from: IndentState, to: IndentState) {
        assert_eq!(self.state, from);
        self.state = to;
    }
}

impl<'a> fmt::Write for IndentedWriter<'a> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        for c in s.chars() {
            match c {
                '\n' => match self.state {
                    IndentState::Pending => {
                        self.w.write_char(c)?;
                    }
                    IndentState::Emitted => {
                        self.w.write_char(c)?;
                        self.transition(IndentState::Emitted, IndentState::Pending);
                    }
                },
                c => match self.state {
                    IndentState::Pending => {
                        self.emit_indent()?;
                        self.w.write_char(c)?;
                    }
                    IndentState::Emitted => {
                        self.w.write_char(c)?;
                    }
                },
            }
        }
        Ok(())
    }
}

pub trait WriteExt {
    fn indented(&mut self) -> IndentedWriter<'_>;
}

impl WriteExt for fmt::Formatter<'_> {
    fn indented(&mut self) -> IndentedWriter<'_> {
        IndentedWriter {
            w: self,
            state: IndentState::Pending,
        }
    }
}

pub enum Visi {
    Pub,
}

impl fmt::Display for Visi {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Pub => write!(f, "pub"),
        }
    }
}

pub enum Repr {
    C,
    I32,
}

impl fmt::Display for Repr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::C => write!(f, "#[repr(C)]"),
            Self::I32 => write!(f, "#[repr(i32)]"),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Unit {
    pub toplevels: Vec<TopLevel>,
}

impl fmt::Display for Unit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for tl in &self.toplevels {
            write!(f, "{}", tl)?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum TopLevel {
    AliasDeclaration(AliasDeclaration),
    StructDeclaration(StructDeclaration),
    EnumDeclaration(EnumDeclaration),
    FunctionDeclaration(FunctionDeclaration),
}

impl From<AliasDeclaration> for TopLevel {
    fn from(d: AliasDeclaration) -> Self {
        Self::AliasDeclaration(d)
    }
}

impl From<StructDeclaration> for TopLevel {
    fn from(d: StructDeclaration) -> Self {
        Self::StructDeclaration(d)
    }
}

impl From<EnumDeclaration> for TopLevel {
    fn from(d: EnumDeclaration) -> Self {
        Self::EnumDeclaration(d)
    }
}

impl From<FunctionDeclaration> for TopLevel {
    fn from(d: FunctionDeclaration) -> Self {
        Self::FunctionDeclaration(d)
    }
}

impl fmt::Display for TopLevel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::AliasDeclaration(d) => {
                write!(f, "{}", d)?;
            }
            Self::StructDeclaration(d) => {
                write!(f, "{}", d)?;
            }
            Self::EnumDeclaration(d) => {
                write!(f, "{}", d)?;
            }
            Self::FunctionDeclaration(d) => {
                write!(f, "{}", d)?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct AliasDeclaration {
    pub name: Identifier,
    pub typ: Type,
}

impl fmt::Display for AliasDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "pub type {name} = {typ};",
            name = self.name,
            typ = self.typ
        )
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct StructDeclaration {
    pub name: Identifier,
    pub fields: Vec<StructField>,
}

impl fmt::Display for StructDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{repr}", repr = Repr::C)?;
        writeln!(
            f,
            "{vis} struct {name} {{",
            vis = Visi::Pub,
            name = self.name
        )?;
        {
            let f = &mut f.indented();
            for field in &self.fields {
                writeln!(f, "{},", field)?;
            }
        }
        writeln!(f, "}}")?;
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct StructField {
    pub name: Identifier,
    pub typ: Type,
}

impl fmt::Display for StructField {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{name}: {typ}", name = self.name, typ = self.typ)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct EnumDeclaration {
    pub name: Identifier,
    pub fields: Vec<EnumField>,
}

impl fmt::Display for EnumDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{repr}", repr = Repr::I32)?;
        writeln!(f, "{vis} enum {name} {{", vis = Visi::Pub, name = self.name)?;
        {
            let f = &mut f.indented();
            for field in &self.fields {
                writeln!(f, "{},", field)?;
            }
        }
        writeln!(f, "}}")?;
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct EnumField {
    pub name: Identifier,
    pub value: Option<Expr>,
}

impl fmt::Display for EnumField {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{name}", name = self.name)?;
        if let Some(value) = &self.value {
            write!(f, " = {value} as i32", value = value.as_enum_expr())?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct FunctionDeclaration {
    pub name: Identifier,
    pub params: Vec<FunctionParam>,
    pub ret: Option<Type>,
}

impl fmt::Display for FunctionDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "extern {c:?} {{", c = "C")?;
        {
            let f = &mut f.indented();
            write!(f, "{vis} fn {name} (", vis = Visi::Pub, name = self.name,)?;
            for (i, param) in self.params.iter().enumerate() {
                if i != 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{param}", param = param)?;
            }
            write!(f, ")")?;
            if let Some(ret) = self.ret.as_ref() {
                write!(f, " -> {ret}", ret = ret)?;
            }
            writeln!(f, ";")?;
        }
        writeln!(f, "}}")?;

        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct FunctionParam {
    pub name: Identifier,
    pub typ: Type,
}

impl fmt::Display for FunctionParam {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{name}: {typ}", name = self.name, typ = self.typ)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Type {
    Name(Identifier),
    Pointer { konst: bool, inner: Box<Type> },
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Name(name) => write!(f, "{}", name),
            Self::Pointer { konst, inner } => match konst {
                true => write!(f, "*const {}", inner),
                false => write!(f, "*mut {}", inner),
            },
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Identifier {
    pub value: String,
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if RUST_KEYWORDS.contains(&self.value) {
            write!(f, r#"r#{value}"#, value = self.value)
        } else {
            write!(f, "{}", self.value)
        }
    }
}

impl Identifier {
    pub fn name(s: &str) -> Self {
        Self {
            value: s.to_string(),
        }
    }

    pub fn struct_name(s: &str) -> Self {
        Self {
            value: format!("struct_{}", s),
        }
    }

    pub fn enum_name(s: &str) -> Self {
        Self {
            value: format!("enum_{}", s),
        }
    }

    pub fn union_name(s: &str) -> Self {
        Self {
            value: format!("union_{}", s),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    Constant(ast::Constant),
    BinaryOperator(ast::BinaryOperator, Box<Expr>, Box<Expr>),
    Cast(Type, Box<Expr>),
    SizeOf(Type),
    AlignOf(Type),
    Identifier(String),
}

struct EnumExpr<'a>(&'a Expr);

impl Expr {
    fn as_enum_expr(&self) -> EnumExpr {
        EnumExpr(self)
    }
}

impl<'a> fmt::Display for EnumExpr<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            Expr::Constant(c) => match c {
                ast::Constant::Integer(ast::Integer { base, number, .. }) => {
                    match base {
                        ast::IntegerBase::Decimal => {}
                        ast::IntegerBase::Octal => write!(f, "0o")?,
                        ast::IntegerBase::Hexadecimal => write!(f, "0x")?,
                    }
                    write!(f, "{}", number)?;
                }
                ast::Constant::Float(ast::Float { base, number, .. }) => {
                    match base {
                        ast::FloatBase::Decimal => {}
                        ast::FloatBase::Hexadecimal => write!(f, "0x")?,
                    }
                    write!(f, "{}", number)?;
                }
                ast::Constant::Character(_) => {}
            },
            Expr::BinaryOperator(op, lhs, rhs) => {
                let lhs = lhs.as_enum_expr();
                let rhs = rhs.as_enum_expr();
                match op {
                    ast::BinaryOperator::Index => write!(f, "({}[{}])", lhs, rhs)?,
                    ast::BinaryOperator::Multiply => write!(f, "({} * {})", lhs, rhs)?,
                    ast::BinaryOperator::Divide => write!(f, "({} / {})", lhs, rhs)?,
                    ast::BinaryOperator::Modulo => write!(f, "({} % {})", lhs, rhs)?,
                    ast::BinaryOperator::Plus => write!(f, "({} + {})", lhs, rhs)?,
                    ast::BinaryOperator::Minus => write!(f, "({} - {})", lhs, rhs)?,
                    ast::BinaryOperator::ShiftLeft => write!(f, "({} << {})", lhs, rhs)?,
                    ast::BinaryOperator::ShiftRight => write!(f, "({} >> {})", lhs, rhs)?,
                    ast::BinaryOperator::Less => write!(f, "({} < {})", lhs, rhs)?,
                    ast::BinaryOperator::Greater => write!(f, "({} > {})", lhs, rhs)?,
                    ast::BinaryOperator::LessOrEqual => write!(f, "({} <= {})", lhs, rhs)?,
                    ast::BinaryOperator::GreaterOrEqual => write!(f, "({} >= {})", lhs, rhs)?,
                    ast::BinaryOperator::Equals => write!(f, "({} == {})", lhs, rhs)?,
                    ast::BinaryOperator::NotEquals => write!(f, "({} != {})", lhs, rhs)?,
                    ast::BinaryOperator::BitwiseAnd => write!(f, "({} & {})", lhs, rhs)?,
                    ast::BinaryOperator::BitwiseXor => write!(f, "({} ^ {})", lhs, rhs)?,
                    ast::BinaryOperator::BitwiseOr => write!(f, "({} | {})", lhs, rhs)?,
                    ast::BinaryOperator::LogicalAnd => write!(f, "({} && {})", lhs, rhs)?,
                    ast::BinaryOperator::LogicalOr => write!(f, "({} || {})", lhs, rhs)?,
                    ast::BinaryOperator::Assign => todo!(),
                    ast::BinaryOperator::AssignMultiply => todo!(),
                    ast::BinaryOperator::AssignDivide => todo!(),
                    ast::BinaryOperator::AssignModulo => todo!(),
                    ast::BinaryOperator::AssignPlus => todo!(),
                    ast::BinaryOperator::AssignMinus => todo!(),
                    ast::BinaryOperator::AssignShiftLeft => todo!(),
                    ast::BinaryOperator::AssignShiftRight => todo!(),
                    ast::BinaryOperator::AssignBitwiseAnd => todo!(),
                    ast::BinaryOperator::AssignBitwiseXor => todo!(),
                    ast::BinaryOperator::AssignBitwiseOr => todo!(),
                }
            }
            Expr::SizeOf(e) => {
                write!(f, "core::mem::size_of::<{}>()", e)?;
            }
            Expr::AlignOf(e) => {
                write!(f, "core::mem::align_of::<{}>()", e)?;
            }
            Expr::Cast(ty, expr) => {
                write!(f, "({expr} as {ty})", expr = expr.as_enum_expr(), ty = ty)?;
            }
            Expr::Identifier(name) => {
                write!(f, "(Self::{name} as i32)", name = name)?;
            }
        };
        Ok(())
    }
}
