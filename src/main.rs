#![feature(inclusive_range_syntax)]

extern crate binaryen;
extern crate byteorder;
#[macro_use]
extern crate enum_primitive;
#[macro_use]
extern crate error_chain;

mod instruction;
mod error;
mod cfg;
mod trans;

pub use error::*;
use trans::trans;

use std::fs::File;
use std::path::Path;

fn read_rom<P: AsRef<Path>>(path: P) -> Result<Vec<u8>> {
    use std::io::Read;

    let mut rom_file = File::open(path)?;
    let mut rom_buffer = Vec::new();
    rom_file.read_to_end(&mut rom_buffer)?;
    Ok(rom_buffer)
}

fn main() {
    use std::env::args;

    let filename = args().nth(1).unwrap();
    build_rom(&filename);
}

pub fn build_rom(filename: &str) {
    let rom_buffer = read_rom(filename).unwrap();
    println!("{:?}", rom_buffer);
    let cfg = cfg::build_cfg(&rom_buffer).unwrap();
    println!("{:#?}", cfg);
    cfg.print();

    trans(&cfg);
}

#[cfg(test)]
mod tests {
    use super::build_rom;
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
            build_rom(&rom_filename);
        }
    }
}
