use std::collections::HashMap;
use std::collections::HashSet;
use instruction::*;
use error::*;
use std::fmt;

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
            // TODO: handle this
            panic!("unimplemented");
        }

        let actual_pc = pc as usize;
        let instruction_word = InstructionWord(BigEndian::read_u16(&self.rom[actual_pc..]));

        Instruction::decode(instruction_word)
    }
}

#[derive(Clone, Debug)]
struct BB {
    insts: Vec<Instruction>,
    start: usize,
    end: usize,
    terminator: Terminator,
}

impl BB {
    fn new(insts: Vec<Instruction>, start: usize, end: usize, terminator: Terminator) -> BB {
        assert!(start <= end);
        BB {
            insts,
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

impl<'a> SubroutineBuilder<'a> {
    fn new(rom: Rom<'a>, addr: usize) -> SubroutineBuilder<'a> {
        SubroutineBuilder {
            rom,
            addr,
            root: addr == 0,
            bbs: Vec::new(),
        }
    }

    fn into_routine(self) -> Routine {
        let mut bbs = HashMap::new();
        for bb in self.bbs {
            let BB {
                insts,
                terminator,
                start,
                ..
            } = bb;
            let basic_block = BasicBlock::new(insts, terminator);
            bbs.insert(BasicBlockId(Addr(start as u16)), basic_block);
        }
        Routine {
            entry: BasicBlockId(Addr(self.addr as u16)),
            bbs,
        }
    }

    fn seal_bb(
        &mut self,
        insts: Vec<Instruction>,
        leader: usize,
        pc: usize,
        terminator: Terminator,
    ) {
        println!("sealing {}, {}", leader, pc);
        let bb = BB::new(insts, leader, pc, terminator);
        self.bbs.push(bb);
    }

    fn find_bb(&self, pc: usize) -> Option<usize> {
        self.bbs
            .iter()
            .position(|bb| bb.start <= pc && bb.end >= pc)
    }

    fn build_bb(&mut self, seen_calls: &mut HashSet<Addr>, mut pc: usize) -> Result<()> {
        println!("building bb from {}...", pc);
        println!("{:?}", self.bbs);

        if let Some(bb_position) = self.find_bb(pc) {
            if self.bbs
                .get(bb_position)
                .expect("we found it already")
                .start == pc
            {
                println!("there is {} already", pc);
                return Ok(());
            }

            let BB {
                mut insts,
                start,
                end,
                terminator,
            } = self.bbs.swap_remove(bb_position);
            println!("spliting bb {}, {}", start, end);

            // Split instructions between BBs.
            let (insts1, insts2) = {
                let splitted = insts.split_off((pc - start) / 2 - 1);
                (insts, splitted)
            };
            let falltrough_addr = pc;

            // TODO: Is it ok?
            self.bbs.push(BB::new(
                insts1,
                start,
                pc - 2,
                Terminator::Jump {
                    target: BasicBlockId(Addr(falltrough_addr as u16)),
                },
            ));
            self.bbs.push(BB::new(insts2, pc, end, terminator));
            return Ok(());
        }

        let mut insts: Vec<Instruction> = Vec::new();

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
                    self.seal_bb(insts, leader, pc, Terminator::Ret);
                    break;
                }
                Jump(addr) => {
                    let jump_pc: usize = (addr.0 - 0x200) as usize;
                    self.seal_bb(
                        insts,
                        leader,
                        pc,
                        Terminator::Jump {
                            target: BasicBlockId(Addr(jump_pc as u16)),
                        },
                    );
                    self.build_bb(seen_calls, jump_pc)?;
                    break;
                }
                Skip(predicate) => {
                    let next = pc + 2;
                    let skip = pc + 4;

                    self.seal_bb(
                        insts,
                        leader,
                        pc,
                        Terminator::Skip {
                            predicate,
                            next: BasicBlockId(Addr(next as u16)),
                            skip: BasicBlockId(Addr(skip as u16)),
                        },
                    );

                    // First we do 'skip', then 'next'.
                    self.build_bb(seen_calls, next)?;
                    self.build_bb(seen_calls, skip)?;
                    break;
                }
                Call(addr) => {
                    seen_calls.insert(addr);
                }
                _ => {}
            }

            insts.push(instruction);
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
    // TODO: 0x200 and 0

    let mut seen_calls = HashSet::new();
    let mut subs = HashMap::new();
    let mut subroutine_stack: Vec<Addr> = Vec::new();

    let start_subroutine_id = Addr(0x200);
    subroutine_stack.push(start_subroutine_id);

    while let Some(subroutine_addr) = subroutine_stack.pop() {
        let mut sub_builder =
            SubroutineBuilder::new(Rom::new(rom), (subroutine_addr.0 - 0x200) as usize);
        let mut seen_calls_from_sr = HashSet::new();
        sub_builder.build_cfg(&mut seen_calls_from_sr)?;
        subs.insert(subroutine_addr.into(), sub_builder);

        for seen in seen_calls_from_sr.difference(&seen_calls).cloned() {
            println!("queueing {:#?}", seen);
            subroutine_stack.push(seen);
        }

        seen_calls = seen_calls
            .intersection(&seen_calls_from_sr)
            .cloned()
            .collect();
    }

    let subroutines = subs.into_iter()
        .map(|(k, v)| (k, v.into_routine()))
        .collect();

    Ok(CFG {
        start: RoutineId(start_subroutine_id),
        subroutines,
    })
}

#[derive(Copy, Clone, Debug)]
pub enum Terminator {
    Ret,
    Jump { target: BasicBlockId },
    Skip {
        predicate: Predicate,
        next: BasicBlockId,
        skip: BasicBlockId,
    },
}

impl Terminator {
    pub fn successors(&self) -> Vec<BasicBlockId> {
        match *self {
            Terminator::Ret => vec![],
            Terminator::Jump { target } => vec![target],
            Terminator::Skip { next, skip, .. } => vec![next, skip],
        }
    }
}

#[derive(Hash, PartialEq, Eq, Debug, Clone, Copy)]
pub struct BasicBlockId(Addr);

impl fmt::Display for BasicBlockId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "bb{}", (self.0).0)
    }
}

#[derive(Hash, PartialEq, Eq, Debug, Clone, Copy)]
pub struct RoutineId(pub Addr);

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

impl Instruction {
    pub fn is_terminating(&self) -> bool {
        use self::Instruction::*;

        match *self {
            Ret | Jump(..) | Skip(..) | JumpPlusV0(..) => true,
            _ => false,
        }
    }
}

impl BasicBlock {
    fn new(insts: Vec<Instruction>, terminator: Terminator) -> BasicBlock {
        assert!(!insts.iter().any(|x| x.is_terminating()));
        BasicBlock { insts, terminator }
    }

    pub fn instructions(&self) -> &[Instruction] {
        &self.insts
    }

    pub fn terminator(&self) -> Terminator {
        self.terminator
    }
}

#[derive(Debug)]
pub struct Routine {
    pub entry: BasicBlockId,
    pub bbs: HashMap<BasicBlockId, BasicBlock>,
}

#[derive(Debug)]
pub struct CFG {
    start: RoutineId,
    subroutines: HashMap<RoutineId, Routine>,
}

impl CFG {
    pub fn start(&self) -> RoutineId {
        self.start
    }

    pub fn subroutines(&self) -> &HashMap<RoutineId, Routine> {
        &self.subroutines
    }

    fn print_bb(
        &self,
        routine: &Routine,
        bb_id: BasicBlockId,
        seen_bbs: &mut HashSet<BasicBlockId>,
    ) {
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
        for subroutine in self.subroutines.values() {
            self.print_routine(subroutine);
        }
    }
}
