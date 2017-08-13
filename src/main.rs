extern crate byteorder;
#[macro_use]
extern crate enum_primitive;
#[macro_use]
extern crate error_chain;
extern crate binaryen;

mod instruction;
mod error;

pub use error::*;

use std::fs::File;
use std::path::Path;
use std::collections::HashMap;
use std::collections::HashSet;
use instruction::*;

#[derive(Clone, Debug)]
enum Terminator {
    Ret,
    Fallthrough { target: usize },
    Jump { target: usize },
    Skip { next: usize, skip: usize },
}

impl Terminator {
    fn successors(&self) -> Vec<usize> {
        match *self {
            Terminator::Ret => vec![],
            Terminator::Fallthrough { target } => vec![target],
            Terminator::Jump { target } => vec![target],
            Terminator::Skip { next, skip } => vec![next, skip],
        }
    }
}

#[derive(Clone, Debug)]
struct BB {
    start: usize,
    end: usize,
    terminator: Terminator,
}

impl BB {
    fn new(start: usize, end: usize, terminator: Terminator) -> BB {
        assert!(start <= end);
        BB {
            start,
            end,
            terminator,
        }
    }
}

struct SubroutineBuilder<'a> {
    rom: &'a [u8],
    addr: usize,
    root: bool,
    bbs: Vec<BB>,
}

impl<'a> SubroutineBuilder<'a> {
    fn new(rom: &'a [u8], addr: usize) -> SubroutineBuilder<'a> {
        SubroutineBuilder {
            rom,
            addr,
            root: addr == 0,
            bbs: Vec::new(),
        }
    }

    fn seal_bb(&mut self, leader: usize, pc: usize, terminator: Terminator) {
        println!("sealing {}, {}", leader, pc);
        self.bbs.push(BB::new(leader, pc, terminator));
    }

    fn find_bb(&self, pc: usize) -> Option<usize> {
        //println!("finding pc = {}", pc);
        self.bbs.iter().position(|ref bb| {
            let result = bb.start <= pc && bb.end >= pc;
            //println!("{:?} = {}", bb, result);
            result
        })
    }

    fn build_bb(&mut self, seen_calls: &mut HashSet<Addr>, mut pc: usize) -> Result<()> {
        println!("building bb from {}...", pc);
        println!("{:?}", self.bbs);
        match self.find_bb(pc) {
            Some(bb_position) => {
                if self.bbs.get(bb_position).unwrap().start == pc {
                    println!("there is {} already", pc);
                    return Ok(());
                }

                let bb = self.bbs.swap_remove(bb_position);
                println!("spliting bb {}, {}", bb.start, bb.end);

                let falltrough_addr = pc;

                // TODO: Is it ok?
                self.bbs.push(BB::new(
                    bb.start,
                    pc - 2,
                    Terminator::Fallthrough { target: falltrough_addr },
                ));
                self.bbs.push(
                    BB::new(pc, bb.end, bb.terminator),
                );
                return Ok(());
            }
            None => {}
        }

        let leader = pc;
        loop {
            if pc >= self.rom.len() {
                panic!("unimplemented");
            }

            let instruction_word = {
                use byteorder::{ByteOrder, BigEndian};
                let actual_pc = pc as usize;
                InstructionWord(BigEndian::read_u16(&self.rom[actual_pc..]))
            };
            let instruction = Instruction::decode(instruction_word)?;

            println!("pc={}, {:?}", pc, instruction);

            use Instruction::*;
            match instruction {
                Ret => {
                    if self.root {
                        panic!("ret in root");
                    }
                    self.seal_bb(leader, pc, Terminator::Ret);
                    break;
                }
                Call(addr) => {
                    seen_calls.insert(addr);
                }
                Jump(addr) => {
                    let jump_pc: usize = (addr.0 - 0x200) as usize;
                    self.seal_bb(leader, pc, Terminator::Jump { target: jump_pc });
                    self.build_bb(seen_calls, jump_pc);
                    break;
                }
                SkipPressed { .. } |
                SkipEqImm { .. } |
                SkipEqReg { .. } => {
                    let next = pc + 2;
                    let skip = pc + 4;

                    self.seal_bb(leader, pc, Terminator::Skip { next, skip });

                    // First we do 'skip', then 'next'.
                    self.build_bb(seen_calls, next);
                    self.build_bb(seen_calls, skip);
                    break;
                }
                _ => {}
            }

            pc += 2;
        }

        Ok(())
    }

    fn build_cfg(&mut self, seen_calls: &mut HashSet<Addr>) -> Result<()> {
        let start_from = self.addr;
        self.build_bb(seen_calls, start_from)?;
        Ok(())
    }

    fn print_bb(&self, printed: &mut HashSet<usize>, addr: usize) {
        println!("bb{}:", addr);
        let bb = self.bbs.iter().find(|ref bb| bb.start == addr).unwrap();
        let mut pc = bb.start;
        loop {
            if pc > bb.end {
                break;
            }

            let instruction_word = {
                use byteorder::{ByteOrder, BigEndian};
                let actual_pc = pc as usize;
                InstructionWord(BigEndian::read_u16(&self.rom[actual_pc..]))
            };
            let instruction = Instruction::decode(instruction_word).unwrap();

            println!("  {}: {:?}", pc, instruction);
            pc += 2;
        }

        printed.insert(addr);

        let terminator = &bb.terminator;
        println!("terminator: {:?}", terminator);

        for successor_addr in terminator.successors() {
            if !printed.contains(&successor_addr) {
                self.print_bb(printed, successor_addr);
            }
        }
    }

    fn print(&self) {
        let mut printed = HashSet::new();
        self.print_bb(&mut printed, self.addr);
    }
}

struct CFGBuilder<'a> {
    rom: &'a [u8],
    start: Option<SubroutineBuilder<'a>>,
    subs: HashMap<Addr, SubroutineBuilder<'a>>,
}

impl<'a> CFGBuilder<'a> {
    fn new(rom: &'a [u8]) -> CFGBuilder<'a> {
        CFGBuilder {
            rom,
            start: None,
            subs: HashMap::new(),
        }
    }

    fn build_cfg(&mut self) -> Result<()> {
        let mut sub_builder = SubroutineBuilder::new(self.rom, 0);
        let mut seen_calls = HashSet::new();
        sub_builder.build_cfg(&mut seen_calls)?;
        self.start = Some(sub_builder);

        let mut subroutine_stack = Vec::new();
        for seen_call in seen_calls.iter().cloned() {
            subroutine_stack.push(seen_call);
        }

        loop {
            let subroutine_addr = match subroutine_stack.pop() {
                Some(addr) => addr,
                None => break,
            };

            let mut sub_builder =
                SubroutineBuilder::new(self.rom, (subroutine_addr.0 - 0x200) as usize);
            let mut seen_calls_from_sr = HashSet::new();
            sub_builder.build_cfg(&mut seen_calls_from_sr)?;
            self.subs.insert(subroutine_addr, sub_builder);

            for seen in seen_calls.difference(&seen_calls_from_sr).cloned() {
                subroutine_stack.push(seen);
            }

            seen_calls = seen_calls
                .intersection(&seen_calls_from_sr)
                .cloned()
                .collect();
        }

        Ok(())
    }

    fn print(&self) {
        if let Some(ref start_sub_builder) = self.start {
            println!("start:");
            start_sub_builder.print();
        }

        for (index, sub) in self.subs.values().enumerate() {
            println!("subroutine {}:", index);
            sub.print();
        }
    }
}

fn build_cfg(rom: &[u8]) -> Result<CFGBuilder> {
    let mut builder = CFGBuilder::new(rom);
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
    use std::io::Read;
    use std::env::args;

    let filename = args().nth(1).unwrap();

    let mut rom_buffer = read_rom(filename).unwrap();
    println!("{:?}", rom_buffer);
    let module = build_cfg(&rom_buffer).unwrap();
    module.print();
}

// stuff

struct TransCtx {
    module: binaryen::ModuleBuilder,
}

impl TransCtx {
    fn new() -> TransCtx {
        TransCtx { module: binaryen::ModuleBuilder::new() }
    }

    fn trans_instruction(&mut self, instruction: Instruction) {
        use Instruction::*;
        match instruction {
            ClearScreen => {}
            Ret => {}
            Sys(_addr) => {}
            Jump(addr) => {}
            Call(addr) => {}
            SkipEqImm { vx, imm, inv } => {}
            SkipEqReg { vx, vy, inv } => {}
            PutImm { vx, imm } => {}
            AddImm { vx, imm } => {}
            Apply { vx, vy, f } => {
                match f {
                    Fun::Id => {}
                    Fun::Or => {}
                    Fun::And => {}
                    Fun::Xor => {}
                    Fun::Add => {}
                    Fun::Subtract => {}
                    Fun::ShiftRight => {}
                    Fun::SubtractInv => {}
                    Fun::ShiftLeft => {}
                }
            }
            SetI(addr) => {}
            JumpPlusV0(_addr) => {}
            Randomize { vx, imm } => {}
            Draw { vx, vy, n } => {}
            SkipPressed { vx, inv } => {}
            GetDT(vx) => {}
            WaitKey(_vx) => {}
            SetDT(vx) => {}
            SetST(vx) => {}
            AddI(vx) => {}
            LoadGlyph(vx) => {}
            StoreBCD(vx) => {}
            StoreRegs(vx) => {}
            LoadRegs(vx) => {}
        }
    }
}
