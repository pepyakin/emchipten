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

use docopt::Docopt;
use std::fs::File;

mod instruction;
mod error;
mod cfg;
mod trans;
pub mod ffi;

pub use error::*;
pub use trans::*;

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

quick_main!(run);

fn run() -> Result<()> {
    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.deserialize())
        .unwrap_or_else(|e| e.exit());

    let opts = Opts {
        optimize: args.flag_optimize,
        print: args.flag_print
    };
    let rom_buffer = read_rom(&args.arg_rom_file)?;
    let wasm = build_rom(&rom_buffer, &opts)?;
    if let Some(out_file) = args.flag_o {
        dump(&wasm, &out_file)?;
    }
    Ok(())
}

fn read_rom(filename: &str) -> Result<Vec<u8>> {
    use std::io::Read;
    let mut rom_file = File::open(filename)?;
    let mut rom_buffer = Vec::new();
    rom_file.read_to_end(&mut rom_buffer)?;
    Ok(rom_buffer)
}

fn dump(buf: &[u8], filename: &str) -> Result<()> {
    use std::fs::File;
    use std::io::Write;
    let mut file = File::create(filename)?;
    file.write_all(buf)?;
    Ok(())
}

pub fn build_rom(rom: &[u8], opts: &Opts) -> Result<Vec<u8>> {
    let cfg = cfg::build_cfg(rom)?;
    trans::trans_rom(rom, &cfg, opts)
}

#[cfg(test)]
mod tests {
    use super::build_rom;
    use error::*;
    use {cfg, trans};
    use std::fs::File;
    use std::path::Path;

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

    fn read_and_build_rom(filename: &str) -> Result<Vec<u8>> {
        use std::io::Read;
        let mut rom_file = File::open(filename)?;
        let mut rom_buffer = Vec::new();
        rom_file.read_to_end(&mut rom_buffer)?;
        build_rom(&rom_buffer, trans::Opts::default())
    }

    #[test]
    fn compile_roms() {
        for rom in TEST_ROMS {
            let rom_filename = format!("roms/{}", rom);
            println!("building {}:", rom);
            read_and_build_rom(&rom_filename);
        }
    }
}
