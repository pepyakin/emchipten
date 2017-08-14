use std::collections::HashMap;
use std::collections::HashSet;
use instruction::*;
use error::*;

fn decode_instruction(rom: &[u8], pc: usize) -> Result<Instruction> {
    use byteorder::{ByteOrder, BigEndian};
    let actual_pc = pc as usize;
    let instruction_word = InstructionWord(BigEndian::read_u16(&rom[actual_pc..]));

    Instruction::decode(instruction_word)
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

impl<'a> From<SubroutineBuilder<'a>> for Routine {
    fn from(builder: SubroutineBuilder<'a>) -> Routine {
        let mut bbs = HashMap::new();
        for bb in builder.bbs {
            let mut insts = Vec::new();
            for pc in (bb.start..bb.end).step_by(2) {
                let inst = decode_instruction(builder.rom, pc).unwrap();
                insts.push(inst);
            }

            let basic_block = BasicBlock {
                insts,
                terminator: bb.terminator
            };
            bbs.insert(BasicBlockId(Addr(bb.start as u16)), basic_block);
        }
        Routine {
            entry: BasicBlockId(Addr(builder.addr as u16)),
            bbs
        }
    }
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
            let instruction = decode_instruction(self.rom, pc)?;

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
                    self.build_bb(seen_calls, jump_pc)?;
                    break;
                }
                SkipPressed { .. } |
                SkipEqImm { .. } |
                SkipEqReg { .. } => {
                    let next = pc + 2;
                    let skip = pc + 4;

                    self.seal_bb(leader, pc, Terminator::Skip { next, skip });

                    // First we do 'skip', then 'next'.
                    self.build_bb(seen_calls, next)?;
                    self.build_bb(seen_calls, skip)?;
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

            let instruction = decode_instruction(self.rom, pc).unwrap();

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

pub fn build_cfg(rom: &[u8]) -> Result<CFG> {
    let mut sub_builder = SubroutineBuilder::new(rom, 0);
    let mut seen_calls = HashSet::new();
    sub_builder.build_cfg(&mut seen_calls)?;
    let start = sub_builder;
    let mut subs = HashMap::new();

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
            SubroutineBuilder::new(rom, (subroutine_addr.0 - 0x200) as usize);
        let mut seen_calls_from_sr = HashSet::new();
        sub_builder.build_cfg(&mut seen_calls_from_sr)?;
        subs.insert(subroutine_addr.into(), sub_builder);

        for seen in seen_calls.difference(&seen_calls_from_sr).cloned() {
            subroutine_stack.push(seen);
        }

        seen_calls = seen_calls
            .intersection(&seen_calls_from_sr)
            .cloned()
            .collect();
    }

    let subroutines = subs.into_iter().map(|(k, v)| { (k, v.into()) }).collect();

    Ok(CFG {
        start: start.into(),
        subroutines
    })
}

#[derive(Clone, Debug)]
pub enum Terminator {
    Ret,
    Fallthrough { target: usize },
    Jump { target: usize },
    Skip { next: usize, skip: usize },
}

impl Terminator {
    pub fn successors(&self) -> Vec<usize> {
        match *self {
            Terminator::Ret => vec![],
            Terminator::Fallthrough { target } => vec![target],
            Terminator::Jump { target } => vec![target],
            Terminator::Skip { next, skip } => vec![next, skip],
        }
    }
}

#[derive(Hash, PartialEq, Eq, Debug, Clone, Copy)]
pub struct BasicBlockId(Addr);

#[derive(Hash, PartialEq, Eq, Debug, Clone, Copy)]
pub struct RoutineId(Addr);

impl From<Addr> for RoutineId {
    fn from(addr: Addr) -> RoutineId {
        RoutineId(addr)
    }
}

impl From<Addr> for BasicBlockId {
    fn from(addr: Addr) -> BasicBlockId {
        BasicBlockId(addr)
    }
}

#[derive(Debug)]
pub struct BasicBlock {
    insts: Vec<Instruction>,
    terminator: Terminator
}

#[derive(Debug)]
pub struct Routine {
    entry: BasicBlockId,
    bbs: HashMap<BasicBlockId, BasicBlock>
}

#[derive(Debug)]
pub struct CFG {
    start: Routine,
    subroutines: HashMap<RoutineId, Routine>
}
