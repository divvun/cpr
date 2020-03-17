#![allow(dead_code)]

mod devenv;
mod parser;
mod translator;

use argh::*;
use parser::Parser;
use std::error::Error;

use std::path::PathBuf;

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
}

fn main() -> Result<(), Box<dyn Error>> {
    env_logger::init();
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

    log::debug!("System paths: {:#?}", system_paths);

    let parser = Parser::new(args.file, system_paths, vec![]).unwrap();

    for (include, unit) in parser.iter() {
        let chunks = unit.chunks()?;
        log::debug!("{:?}: {} chunks", include, chunks.len());
        for chunk in chunks {
            println!("{:#?}", chunk);
        }
    }

    Ok(())
}

use ctor::ctor;

#[ctor]
fn init_color_backtrace() {
    color_backtrace::install();
}
