#![allow(dead_code)]

mod devenv;
mod frontend;
mod translator;

use argh::*;
use frontend::{grammar::Include, Parser, SourceDir};
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
        .or_else(devenv::get_kits_path)
        .expect("Windows 10 Kit include path should be autodetected or specified with --kits-path");

    let msvc_path = args
        .msvc_path
        .or_else(devenv::get_msvc_path)
        .expect("MSVC include path should be autodetected or specified with --msvc-path");

    let system_dirs = vec![
        SourceDir {
            pkg: "ucrt".into(),
            path: kits.join("ucrt"),
        },
        SourceDir {
            pkg: "shared".into(),
            path: kits.join("shared"),
        },
        SourceDir {
            pkg: "um".into(),
            path: kits.join("um"),
        },
        SourceDir {
            pkg: "km".into(),
            path: kits.join("km"),
        },
        SourceDir {
            pkg: "vc".into(),
            path: msvc_path,
        },
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

    log::info!("System dirs: {:#?}", system_dirs);
    let provider = frontend::FileSourceProvider::new(system_dirs);

    let root_file = &args.file;
    let root_parent = root_file.parent().unwrap();
    let root_source_dir = SourceDir {
        pkg: root_parent
            .file_name()
            .unwrap()
            .to_str()
            .unwrap()
            .to_string(),
        path: root_parent.to_path_buf(),
    };
    let root_include = Include::Quoted(PathBuf::from(root_file.file_name().unwrap()));

    let mut parser = Parser::new(Box::new(provider), ctx, Env::with_msvc());
    let root_id = parser
        .provider
        .resolve(&mut parser.idgen, &root_source_dir, &root_include)?;

    parser.parse_file(root_id)?;
    log::info!("Done parsing!");

    let config = translator::Config { arch };

    use std::{fs, io::Write};
    let manifest_path = args.output.join("Cargo.toml");
    fs::create_dir_all(manifest_path.parent().unwrap())?;
    std::fs::write(
        &manifest_path,
        format!(
            r#"
[package]
name = "{crate_name}"
version = "0.1.0"
authors = []
edition = "2018"

# workaround for cpr itself being a workspace
[workspace]
"#,
            crate_name = root_source_dir.pkg
        ),
    )?;

    let top_level_path = args.output.join("src").join("lib.rs");
    fs::create_dir_all(top_level_path.parent().unwrap())?;
    let mut top_level = fs::File::create(&top_level_path)?;

    for incl in &parser.ordered_files {
        let unit = parser.units.get(incl).unwrap();
        let file_info = parser.provider.info(unit.id).unwrap();

        if unit.declarations.is_empty() {
            println!("{} | skipping (no decls)", file_info.path);
            continue;
        }

        let trans_unit = translator::translate_unit(
            &config,
            parser.provider.as_ref(),
            unit.id,
            &unit.declarations,
        );
        let pkg_components = file_info.path.pkg_components();
        let stem = pkg_components.last().unwrap();

        // TODO: this is all kinds of wrong
        writeln!(top_level, "pub mod {};", stem)?;
        writeln!(top_level, "pub use {}::*;", stem)?;

        let mut out_path = args.output.join("src").join(stem);
        out_path.set_extension("rs");

        fs::create_dir_all(out_path.parent().unwrap())?;
        let mut f = fs::File::create(&out_path)?;
        writeln!(f, "{}", translator::prelude())?;
        writeln!(f, "// @generated from {:?}", file_info.path.source_path())?;
        writeln!(f)?;

        writeln!(f, "pub use super::*;")?;
        write!(f, "{}", trans_unit)?;
        println!(
            "{} | ({} C => {} Rust) => {}",
            file_info.path,
            unit.declarations.len(),
            trans_unit.toplevels.len(),
            out_path.display()
        );
    }

    Ok(())
}

use ctor::ctor;
use lang_c::env::Env;

#[ctor]
fn install_extensions() {
    color_backtrace::install();
    pretty_env_logger::init();
}
