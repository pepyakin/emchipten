extern crate byteorder;
#[macro_use]
extern crate enum_primitive;
#[macro_use]
extern crate error_chain;
extern crate binaryen;

mod instruction;
mod error;
mod cfg;

pub use error::*;

use std::fs::File;
use std::path::Path;
use instruction::*;

fn build_cfg(rom: &[u8]) -> Result<cfg::CFGBuilder> {
    let mut builder = cfg::CFGBuilder::new(rom);
    builder.build_cfg()?;
    Ok(builder)
}

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

    let rom_buffer = read_rom(filename).unwrap();
    println!("{:?}", rom_buffer);
    let module = build_cfg(&rom_buffer).unwrap();
    module.print();
}
