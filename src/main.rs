#![feature(inclusive_range_syntax)]

extern crate binaryen;
extern crate byteorder;
extern crate docopt;
#[macro_use]
extern crate enum_primitive;
#[macro_use]
extern crate error_chain;
#[macro_use]
extern crate serde_derive;

mod instruction;
mod error;
mod cfg;
mod trans;

pub use error::*;

use std::fs::File;
use std::path::Path;

use docopt::Docopt;

const USAGE: &'static str = "
emchipten - compile CHIP-8 into WebAssembly.

Usage:
  emchipten [-o <out>|--optimize|--print] <rom-file>

Options:
  -h, --help      Show this screen.
  -o <out>        Output file
  --optimize      Optimize resulting rom
  --print         Print compiled wasm s-expressions
";

#[derive(Debug, Deserialize)]
struct Args {
    flag_o: Option<String>,
    arg_rom_file: String,
    flag_optimize: bool,
    flag_print: bool,
}

fn read_rom<P: AsRef<Path>>(path: P) -> Result<Vec<u8>> {
    use std::io::Read;

    let mut rom_file = File::open(path)?;
    let mut rom_buffer = Vec::new();
    rom_file.read_to_end(&mut rom_buffer)?;
    Ok(rom_buffer)
}

fn main() {
    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.deserialize())
        .unwrap_or_else(|e| e.exit());

    let opts = trans::Opts {
        optimize: args.flag_optimize,
        print: args.flag_print,
        out_file: args.flag_o.as_ref().map(|s| s.as_ref()),
        ..Default::default()
    };
    println!("{:?}", opts);
    build_rom(&args.arg_rom_file, opts);
}

pub fn build_rom(filename: &str, opts: trans::Opts) {
    let rom_buffer = read_rom(filename).unwrap();
    println!("{:?}", rom_buffer);
    let cfg = cfg::build_cfg(&rom_buffer).unwrap();
    println!("{:#?}", cfg);
    cfg.print();

    trans::trans_rom(&rom_buffer, &cfg, opts);
}

#[cfg(test)]
mod tests {
    use super::{build_rom, trans};
    const TEST_ROMS: &[&'static str] = &[
        "15PUZZLE",
        "BLINKY",
        "15PUZZLE",
        "BLINKY",
        "BLITZ",
        "BRIX",
        "CONNECT4",
        "GUESS",
        "HIDDEN",
        // "INVADERS", endless loop
        "KALEID",
        "MAZE",
        "MERLIN",
        "MISSILE",
        "PONG",
        "PONG2",
        "PUZZLE",
        "SYZYGY",
        "TANK",
        "TETRIS",
        "TICTAC",
        "UFO",
        "VBRIX",
        "VERS",
        "WIPEOFF",
        // "ZERO", unexpected EOF
    ];

    #[test]
    fn compile_roms() {
        for rom in TEST_ROMS {
            let rom_filename = format!("roms/{}", rom);
            println!("building {}:", rom);
            let opts = trans::Opts {
                ..Default::default()
            };
            build_rom(&rom_filename, opts);
        }
    }
}
