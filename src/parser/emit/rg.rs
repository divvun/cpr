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
    for kw in source.split(" ") {
        println!("Reserved keyword: {:#?}", kw);
        set.insert(kw.to_string());
    }
    set
});

pub const INDENT: &str = "    "; // 4 spaces

pub(crate) struct IndentedWriter<'a> {
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

pub(crate) trait WriteExt {
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

pub(crate) enum Visi {
    PubCrate,
}

impl fmt::Display for Visi {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::PubCrate => write!(f, "pub(crate)"),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct Unit {
    pub(crate) toplevels: Vec<TopLevel>,
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
pub(crate) enum TopLevel {
    AliasDeclaration(AliasDeclaration),
    StructDeclaration(StructDeclaration),
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
            Self::FunctionDeclaration(d) => {
                write!(f, "{}", d)?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct AliasDeclaration {
    pub(crate) name: Identifier,
    pub(crate) typ: Type,
}

impl fmt::Display for AliasDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "type {name} = {typ};", name = self.name, typ = self.typ)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct StructDeclaration {
    pub(crate) name: Identifier,
    pub(crate) fields: Vec<StructField>,
}

impl fmt::Display for StructDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "[repr(C)]")?;
        writeln!(
            f,
            "{vis} struct {name} {{",
            vis = Visi::PubCrate,
            name = &self.name
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
pub(crate) struct StructField {
    pub(crate) name: Identifier,
    pub(crate) typ: Type,
}

impl fmt::Display for StructField {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{name}: {typ}", name = self.name, typ = self.typ)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct FunctionDeclaration {
    pub(crate) name: Identifier,
    pub(crate) params: Vec<FunctionParam>,
    pub(crate) ret: Option<Type>,
}

impl fmt::Display for FunctionDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "extern {c:?} {{", c = "C")?;
        {
            let f = &mut f.indented();
            write!(
                f,
                "{vis} fn {name} (",
                vis = Visi::PubCrate,
                name = self.name,
            )?;
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
pub(crate) struct FunctionParam {
    pub(crate) name: Identifier,
    pub(crate) typ: Type,
}

impl fmt::Display for FunctionParam {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{name}: {typ}", name = self.name, typ = self.typ)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum Type {
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
pub(crate) struct Identifier {
    pub(crate) value: String,
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
    pub(crate) fn name(s: &str) -> Self {
        Self {
            value: s.to_string(),
        }
    }

    pub(crate) fn struct_name(s: &str) -> Self {
        Self {
            value: format!("struct_{}", s),
        }
    }
}
