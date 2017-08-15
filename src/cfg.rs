use std::collections::HashMap;
use std::collections::HashSet;
use instruction::*;
use error::*;

struct Rom<'a> {
    rom: &'a [u8],
}

impl<'a> Rom<'a> {
    fn new(rom: &[u8]) -> Rom {
        Rom { rom }
    }

    fn decode_instruction(&self, pc: usize) -> Result<Instruction> {
        use byteorder::{BigEndian, ByteOrder};

        if pc >= self.rom.len() {
            panic!("unimplemented");
        }

        let actual_pc = pc as usize;
        let instruction_word = InstructionWord(BigEndian::read_u16(&self.rom[actual_pc..]));

        Instruction::decode(instruction_word)
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
    rom: Rom<'a>,
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
                let inst = builder.rom.decode_instruction(pc).unwrap();
                insts.push(inst);
            }

            let basic_block = BasicBlock {
                insts,
                terminator: bb.terminator,
            };
            bbs.insert(BasicBlockId(Addr(bb.start as u16)), basic_block);
        }
        Routine {
            entry: BasicBlockId(Addr(builder.addr as u16)),
            bbs,
        }
    }
}

impl<'a> SubroutineBuilder<'a> {
    fn new(rom: Rom<'a>, addr: usize) -> SubroutineBuilder<'a> {
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
                    Terminator::Fallthrough {
                        target: BasicBlockId(Addr(falltrough_addr as u16)),
                    },
                ));
                self.bbs.push(BB::new(pc, bb.end, bb.terminator));
                return Ok(());
            }
            None => {}
        }

        let leader = pc;
        loop {
            let instruction = self.rom.decode_instruction(pc)?;

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
                    self.seal_bb(leader, pc, Terminator::Jump { target: BasicBlockId(Addr(jump_pc as u16)) });
                    self.build_bb(seen_calls, jump_pc)?;
                    break;
                }
                SkipPressed { .. } | SkipEqImm { .. } | SkipEqReg { .. } => {
                    let next = pc + 2;
                    let skip = pc + 4;

                    self.seal_bb(leader, pc, Terminator::Skip { next: BasicBlockId(Addr(next as u16)), skip: BasicBlockId(Addr(skip as u16)) });

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
}

pub fn build_cfg(rom: &[u8]) -> Result<CFG> {
    let mut sub_builder = SubroutineBuilder::new(Rom::new(rom), 0);
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
            SubroutineBuilder::new(Rom::new(rom), (subroutine_addr.0 - 0x200) as usize);
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

    let subroutines = subs.into_iter().map(|(k, v)| (k, v.into())).collect();

    Ok(CFG {
        start: start.into(),
        subroutines,
    })
}

#[derive(Clone, Debug)]
pub enum Terminator {
    Ret,
    Fallthrough { target: BasicBlockId },
    Jump { target: BasicBlockId },
    Skip { next: BasicBlockId, skip: BasicBlockId },
}

impl Terminator {
    pub fn successors(&self) -> Vec<BasicBlockId> {
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
    terminator: Terminator,
}

#[derive(Debug)]
pub struct Routine {
    entry: BasicBlockId,
    bbs: HashMap<BasicBlockId, BasicBlock>,
}

#[derive(Debug)]
pub struct CFG {
    start: Routine,
    subroutines: HashMap<RoutineId, Routine>,
}

impl CFG {

    fn print_bb(&self, routine: &Routine, bb_id: BasicBlockId, seen_bbs: &mut HashSet<BasicBlockId>) {
        println!("bb{:?}:", (bb_id.0).0);
        let bb = &routine.bbs[&bb_id];
        for inst in &bb.insts {
            println!("  {:?}", inst);
        }
        println!("  terminator: {:?}", bb.terminator);

        seen_bbs.insert(bb_id);

        for successor_bb in bb.terminator.successors() {
            if !seen_bbs.contains(&successor_bb) {
                self.print_bb(routine, successor_bb, seen_bbs);
            }
        }
    }

    fn print_routine(&self, routine: &Routine) {
        let mut seen_bbs = HashSet::new();
        self.print_bb(routine, routine.entry, &mut seen_bbs);
    }

    pub fn print(&self) {
        self.print_routine(&self.start);
        for subroutine in self.subroutines.values() {
            self.print_routine(subroutine);
        }
    }
}
