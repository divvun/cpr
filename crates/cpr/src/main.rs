#![allow(dead_code)]

mod devenv;
mod frontend;
mod translator;

use argh::*;
use frontend::Parser;
use std::{error::Error, path::PathBuf};

#[derive(FromArgs)]
/// Parse a C header file and its includes and dump info about it
struct Args {
    /// A C header file to parse
    #[argh(positional)]
    file: PathBuf,

    /// windows 10 Kit include dir, something like:
    /// 'C:\Program Files (x86)\Windows Kits\10\Include\10.0.18362.0'
    #[argh(option)]
    kits_path: Option<PathBuf>,

    /// msvc include dir, something like:
    /// 'C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\VC\Tools\MSVC\14.23.28105\include'
    #[argh(option)]
    msvc_path: Option<PathBuf>,

    /// predefine a name
    #[argh(option, short = 'D')]
    defines: Vec<String>,

    /// path of the crate to generate
    #[argh(option, short = 'o')]
    output: PathBuf,

    /// target architecture
    #[argh(option)]
    arch: Option<translator::Arch>,
}

fn main() -> Result<(), Box<dyn Error>> {
    let args: Args = argh::from_env();

    let kits = args
        .kits_path
        .or(devenv::get_kits_path())
        .expect("Windows 10 Kit include path should be autodetected or specified with --kits-path");

    let msvc_path = args
        .msvc_path
        .or(devenv::get_msvc_path())
        .expect("MSVC include path should be autodetected or specified with --msvc-path");

    let system_paths = vec![
        kits.join("ucrt"),
        kits.join("shared"),
        kits.join("um"),
        kits.join("km"),
        msvc_path,
        PathBuf::from(r"."),
    ];

    let arch = args.arch.unwrap_or_default();

    let mut ctx = frontend::Context::new();
    match arch {
        translator::Arch::X86 => {
            for &s in &["_X86", "_M_X86", "_WIN32"] {
                ctx.simple_define(s);
            }
        }
        translator::Arch::X86_64 => {
            for &s in &["_AMD64_", "_M_AMD64", "_WIN32", "_WIN64"] {
                ctx.simple_define(s);
            }
        }
    }

    log::info!("System paths: {:#?}", system_paths);
    let parser = Parser::new(args.file, system_paths, vec![], ctx).unwrap();
    log::info!("Done parsing!");

    let config = translator::Config { arch };

    use std::{fs, io::Write};
    let manifest_path = args.output.join("Cargo.toml");
    fs::create_dir_all(manifest_path.parent().unwrap())?;
    use indoc::indoc;
    std::fs::write(
        &manifest_path,
        indoc!(
            r#"
            [package]
            name = "bindings"
            version = "0.1.0"
            authors = ["Jane Doe <jane@example.org>"]
            edition = "2018"

            # workaround for cpr itself being a workspace
            [workspace]
            "#
        ),
    )?;

    let top_level_path = args.output.join("src").join("lib.rs");
    fs::create_dir_all(top_level_path.parent().unwrap())?;
    let mut top_level = fs::File::create(&top_level_path)?;

    for incl in &parser.ordered_includes {
        let unit = parser.units.get(incl).unwrap();
        if unit.declarations.is_empty() {
            println!("{} | skipping (no decls)", unit.path.display());
            continue;
        }

        let trans_unit = translator::translate_unit(&config, &unit.path, &unit.declarations);

        use frontend::grammar::Include;
        let stem = match incl {
            Include::System(p) => p,
            Include::Quoted(p) => p,
            _ => todo!(),
        }
        .file_stem()
        .unwrap();

        // TODO: this is all kinds of wrong
        writeln!(top_level, "pub mod {};", stem.to_string_lossy())?;
        writeln!(top_level, "pub use {}::*;", stem.to_string_lossy())?;

        let mut out_path = args.output.join("src").join(stem);
        out_path.set_extension("rs");

        fs::create_dir_all(out_path.parent().unwrap())?;
        let mut f = fs::File::create(&out_path)?;
        writeln!(f, "{}", translator::prelude())?;
        writeln!(f, "// @generated from {:?}", unit.path)?;
        writeln!(f)?;

        writeln!(f, "pub use super::*;")?;
        write!(f, "{}", trans_unit)?;
        println!(
            "{} | ({} C => {} Rust) => {}",
            unit.path.display(),
            unit.declarations.len(),
            trans_unit.toplevels.len(),
            out_path.display()
        );
    }

    Ok(())
}

use ctor::ctor;

#[ctor]
fn install_extensions() {
    color_backtrace::install();
    pretty_env_logger::init();
}
