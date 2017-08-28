use std::collections::HashMap;
use std::collections::HashSet;
use instruction::*;
use error::*;
use std::fmt;

// struct Pc(u16);
// TODO: addr to pc  (pc = addr - 0x200)

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

#[derive(Copy, Clone, Debug)]
struct BBRange {
    start: usize,
    end: usize,
}

impl BBRange {
    fn new(start: usize, end: usize) -> BBRange {
        assert!(start <= end);
        BBRange { start, end }
    }

    fn contains(&self, pc: usize) -> bool {
        self.start <= pc && self.end >= pc
    }

    fn intersects(&self, other: &BBRange) -> bool {
        self.start <= other.end && other.start <= self.end
    }
}

struct SubroutineBuilder<'a> {
    rom: Rom<'a>,
    addr: usize,
    root: bool,
    bbs: BasicBlocks,
    bb_ranges: HashMap<BasicBlockId, BBRange>,
}

impl<'a> SubroutineBuilder<'a> {
    fn new(rom: Rom<'a>, addr: usize) -> SubroutineBuilder<'a> {
        SubroutineBuilder {
            rom,
            addr,
            root: addr == 0,
            bbs: BasicBlocks::new(),
            bb_ranges: HashMap::new(),
        }
    }

    fn into_routine(self) -> Routine {
        println!("result = {:#?}", self.bbs);
        Routine {
            entry: BasicBlockId::entry(),
            bbs: self.bbs.into_map(),
        }
    }

    fn seal_bb(
        &mut self,
        id: BasicBlockId,
        range: BBRange,
        insts: Vec<Instruction>,
        terminator: Terminator,
    ) {
        println!("sealing {:?}, bb={:?}", range, id);

        let intersecting_ranges: Vec<_> = self.bb_ranges
            .values()
            .filter(|r| r.intersects(&range))
            .cloned()
            .collect();
        if !intersecting_ranges.is_empty() {
            panic!("there are intersecting ranges: {:#?}", intersecting_ranges);
        }

        let bb = BasicBlock::new(insts, terminator);
        self.bbs.insert(id, bb);
        self.bb_ranges.insert(id, range);
    }

    fn find_bb(&self, pc: usize) -> Option<BasicBlockId> {
        self.bb_ranges
            .iter()
            .find(|&(&id, ref range)| range.contains(pc))
            .map(|(&id, _)| id)
    }

    fn build_bb(
        &mut self,
        pc: &mut usize,
        seen_calls: &mut HashSet<Addr>,
    ) -> Result<(Vec<Instruction>, Instruction)> {
        let mut insts: Vec<Instruction> = Vec::new();
        loop {
            let instruction = self.rom.decode_instruction(*pc)?;

            println!("pc={}, {:?}", pc, instruction);

            if let Instruction::Call(addr) = instruction {
                seen_calls.insert(addr);
            }
            if instruction.is_terminating() {
                return Ok((insts, instruction));
            }

            insts.push(instruction);
            *pc += 2;
        }
    }

    fn find_or_create_bb(
        &mut self,
        current_bb_id: BasicBlockId,
        start_pc: usize,
        target_pc: usize,
    ) -> BasicBlockId {
        if target_pc == start_pc {
            current_bb_id
        } else {
            self.find_bb(target_pc).unwrap_or_else(|| self.bbs.gen_id())
        }
    }

    fn identify_leaders(&mut self, seen_calls: &mut HashSet<Addr>) -> Result<HashMap<usize, BasicBlockId>> {
        let mut leaders: HashMap<usize, BasicBlockId> = HashMap::new();

        let mut stack: Vec<usize> = Vec::new();
        stack.push(self.addr);
        leaders.insert(self.addr, BasicBlockId::entry());

        while let Some(mut pc) = stack.pop() {
            loop {
                let instruction = self.rom.decode_instruction(pc)?;
                match instruction {
                    Instruction::Ret => {
                        break;
                    }
                    Instruction::Jump(addr) => {
                        let target_pc = (addr.0 - 0x200) as usize;
                        if !leaders.contains_key(&target_pc) {
                            leaders.insert(target_pc, self.bbs.gen_id());
                            stack.push(target_pc);
                        }
                        break;
                    }
                    Instruction::Skip(_) => {
                        let next_pc = pc + 2;
                        let skip_pc = pc + 4;

                        if !leaders.contains_key(&next_pc) {
                            leaders.insert(next_pc, self.bbs.gen_id());
                            stack.push(next_pc);
                        }
                        if !leaders.contains_key(&skip_pc) {
                            leaders.insert(skip_pc, self.bbs.gen_id());
                            stack.push(skip_pc);
                        }
                        break;
                    }
                    Instruction::Call(addr) => {
                        seen_calls.insert(addr);
                    }
                    _ => {}
                }
                pc += 2;
            }
        }

        Ok(leaders)
    }

    fn build_cfg(&mut self, seen_calls: &mut HashSet<Addr>) -> Result<()> {
        let leaders = self.identify_leaders(seen_calls)?;

        println!("leaders = {:?}", leaders);

        for leader_pc in leaders.keys().cloned() {
            let bb_id = leaders[&leader_pc];
            let mut pc: usize = leader_pc;
            let mut instructions = Vec::new();
            loop {
                let instruction = self.rom.decode_instruction(pc)?;

                println!("pc={}, {:?}", pc, instruction);

                match instruction {
                    Instruction::Ret => {
                        if self.root {
                            // TODO: error
                            // TODO: is it really belongs to here? Mb .has_ret = true?
                            panic!("ret in root");
                        }
                        self.seal_bb(bb_id, BBRange::new(leader_pc, pc), instructions, Terminator::Ret);
                        break;
                    }
                    Instruction::Jump(addr) => {
                        let target_pc = (addr.0 - 0x200) as usize;
                        let target_id = leaders[&target_pc];
                        self.seal_bb(
                            bb_id,
                            BBRange::new(leader_pc, pc),
                            instructions,
                            Terminator::Jump { target: target_id },
                        );
                        break;
                    }
                    Instruction::Skip(predicate) => {
                        let next_pc = pc + 2;
                        let skip_pc = pc + 4;
                        let next_id = leaders[&next_pc];
                        let skip_id = leaders[&skip_pc];

                        self.seal_bb(
                            bb_id,
                            BBRange::new(leader_pc, pc),
                            instructions,
                            Terminator::Skip {
                                predicate,
                                next: next_id,
                                skip: skip_id,
                            },
                        );
                        break;
                    }
                    _ => {}
                }

                instructions.push(instruction);

                if let Some(next_block_id) = leaders.get(&(pc + 2)) {
                    self.seal_bb(
                        bb_id,
                        BBRange::new(leader_pc, pc),
                        instructions,
                        Terminator::Jump { target: *next_block_id },
                    );
                    break;
                }

                pc += 2;
            }
        }

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

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
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
pub struct BasicBlockId(usize);

impl BasicBlockId {
    fn entry() -> BasicBlockId {
        BasicBlockId(0)
    }
}

impl fmt::Display for BasicBlockId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "bb{}", self.0)
    }
}

#[derive(Hash, PartialEq, Eq, Debug, Clone, Copy)]
pub struct RoutineId(pub Addr);

impl From<Addr> for RoutineId {
    fn from(addr: Addr) -> RoutineId {
        RoutineId(addr)
    }
}

#[derive(Debug)]
pub struct BasicBlock {
    insts: Vec<Instruction>,
    terminator: Terminator,
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

    pub fn split(&mut self, split_offset: usize, second_bb_id: BasicBlockId) -> BasicBlock {
        // Split instructions between BBs.
        let insts2 = self.insts.split_off(split_offset);
        let bb2 = BasicBlock::new(insts2, self.terminator);

        self.terminator = Terminator::Jump {
            target: second_bb_id,
        };

        bb2
    }
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

#[derive(Debug)]
struct BasicBlocks {
    id_counter: usize,
    bbs: HashMap<BasicBlockId, BasicBlock>,
}

impl BasicBlocks {
    fn new() -> BasicBlocks {
        BasicBlocks {
            id_counter: 1, // 0 reserved for BasicBlockId::entry()
            bbs: HashMap::new(),
        }
    }

    fn gen_id(&mut self) -> BasicBlockId {
        let id = self.id_counter;
        self.id_counter += 1;
        BasicBlockId(id)
    }

    fn insert(&mut self, id: BasicBlockId, basic_block: BasicBlock) {
        let existing_value = self.bbs.insert(id, basic_block);
        assert!(existing_value.is_none());
    }

    fn split(&mut self, id: BasicBlockId, offspring_bb_id: BasicBlockId, split_offset: usize) {
        // TODO: inline splint fn?
        let offspring_bb = self.bbs
            .get_mut(&id)
            .expect("specified BasicBlock should be inserted")
            .split(split_offset, offspring_bb_id);
        self.insert(offspring_bb_id, offspring_bb);
    }

    fn into_map(self) -> HashMap<BasicBlockId, BasicBlock> {
        if self.bbs.is_empty() {
            return self.bbs;
        }

        self.bbs
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
        println!("{}:", bb_id);
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

pub fn inline(mut cfg: CFG) -> Routine {
    // Нужно пойти с Start routine, инструкция за инструкцией.
    // Как только обнаружим call F:
    // - нужно разделить текущий BB(bs, be) на BB1(bs, call_pc - 2) и exit: BB2(call_pc + 2, be)
    // - вставить все F.bbs в start.bbs
    // - пройтись по всем F.bbs, если terminator == Ret нужно его изменить на Jump { target: exit }
    //
    // NOTE: BBID могут колизиться?

    let mut bb_queue = Vec::new();

    let start_routine_id = cfg.start;
    let start_routine = cfg.subroutines
        .get_mut(&start_routine_id)
        .expect("start should be inside subroutines map");
    bb_queue.push(start_routine.entry);

    while let Some(bb_id) = bb_queue.pop() {
        let mut bb = start_routine.bbs.remove(&bb_id).unwrap();

        fn is_call(inst: &Instruction) -> bool {
            if let &Instruction::Call(_) = inst {
                true
            } else {
                false
            }
        }

        if let Some(call_position) = bb.instructions().iter().position(is_call) {
            // let bb2 = bb.split(call_position);

            // start_routine.bbs.insert(bb_id, bb);
            // start_routine.bbs.insert(panic!(), bb2); // TODO:
        } else {
            start_routine.bbs.insert(bb_id, bb);
        }
    }

    panic!()
}

#[test]
fn test_bbrange_contains() {
    let bb = BBRange::new(0, 2);
    assert!(bb.contains(0));
    assert!(bb.contains(1));
    assert!(bb.contains(2));
    assert!(!bb.contains(3));
}

#[test]
fn test_bbrange_intersects() {
    assert!(BBRange::new(0, 3).intersects(&BBRange::new(1, 4))); // intersection 1-3
    assert!(BBRange::new(0, 3).intersects(&BBRange::new(0, 1))); // intersection 0-1
    assert!(BBRange::new(0, 0).intersects(&BBRange::new(0, 0))); // intersection 0-0
    assert!(BBRange::new(5, 10).intersects(&BBRange::new(6, 7))); // intersection 6-7
    assert!(!BBRange::new(0, 5).intersects(&BBRange::new(6, 10))); // intersection 6-7
}

#[test]
fn test_jump_self() {
    #[cfg_attr(rustfmt, rustfmt_skip)]
    let rom = vec![
        0x00, 0xE0, // CLS
        0x12, 0x02, // 0x202: JMP 0x202
    ];
    let cfg = build_cfg(&rom).unwrap();

    let start = cfg.start;
    let start_routine = cfg.subroutines.get(&start).unwrap();

    println!("{:#?}", start_routine);

    let entry = start_routine.entry;
    let entry_bb = &start_routine.bbs[&entry];

    assert_eq!(entry_bb.insts, vec![Instruction::ClearScreen]);
}

#[test]
fn test_jump_self2() {
    #[cfg_attr(rustfmt, rustfmt_skip)]
    let rom = vec![
        0x12, 0x02, // JMP 0x202
        0x12, 0x02, // 0x202: JMP 0x202
    ];
    let cfg = build_cfg(&rom).unwrap();

    let start = cfg.start;
    let start_routine = cfg.subroutines.get(&start).unwrap();

    println!("{:#?}", start_routine);

    let entry = start_routine.entry;
    let entry_bb = &start_routine.bbs[&entry];

    assert_eq!(entry_bb.insts, vec![]);
    assert_eq!(
        entry_bb.terminator,
        Terminator::Jump {
            target: BasicBlockId(1),
        }
    );
}

#[test]
fn test_cfg() {
    #[cfg_attr(rustfmt, rustfmt_skip)]
    let rom = vec![
        0x30, 0x02, // 0x200: SE V0, 0
        0x12, 0x08, // JMP 0x208
        0x12, 0x06, // JMP 0x206
        0x00, 0x01, // 0x206: SYS 0x001
        0x00, 0x02, // 0x208: SYS 0x002,
        0x12, 0x00, // JMP 0x200
    ];
    let cfg = build_cfg(&rom).unwrap();

    let start = cfg.start;
    let start_routine = cfg.subroutines.get(&start).unwrap();

    println!("{:#?}", start_routine);
}
