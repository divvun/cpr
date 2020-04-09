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

    log::info!("System paths: {:#?}", system_paths);
    let parser = Parser::new(args.file, system_paths, vec![]).unwrap();
    log::info!("Done parsing!");

    for incl in &parser.ordered_includes {
        let unit = parser.units.get(incl).unwrap();
        if unit.declarations.is_empty() {
            continue;
        }
        println!();
        println!("================================");
        println!("{:?}: {} declarations", incl, unit.declarations.len());

        let config = translator::Config {
            arch: args.arch.unwrap_or_default(),
        };
        let unit = translator::translate_unit(config, &unit.declarations);
        println!("{}", unit);
    }

    Ok(())
}

use ctor::ctor;

#[ctor]
fn install_extensions() {
    color_backtrace::install();
    pretty_env_logger::init();
}
