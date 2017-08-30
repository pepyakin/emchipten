use std::collections::HashMap;
use std::collections::HashSet;
use instruction::*;
use error::*;
use std::fmt;

const ROM_START_ADDR: usize = 0x200;

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug, PartialOrd, Ord)]
struct Pc(usize);

impl Pc {
    fn advance(self, n: usize) -> Pc {
        Pc(self.0 + n * 2)
    }
}

impl From<Addr> for Pc {
    fn from(addr: Addr) -> Pc {
        Pc(addr.0 as usize - ROM_START_ADDR)
    }
}

struct Rom<'a> {
    bytes: &'a [u8],
}

impl<'a> Rom<'a> {
    fn new(bytes: &[u8]) -> Rom {
        Rom { bytes }
    }

    fn decode_instruction(&self, pc: Pc) -> Result<Instruction> {
        use byteorder::{BigEndian, ByteOrder};

        if pc.0 >= self.bytes.len() {
            // TODO: handle this
            panic!("unimplemented");
        }

        let Pc(actual_pc) = pc;
        let instruction_word = InstructionWord(BigEndian::read_u16(&self.bytes[actual_pc..]));

        Instruction::decode(instruction_word)
    }
}

#[derive(Copy, Clone, Debug)]
struct BBRange {
    start: Pc,
    end: Pc,
}

impl BBRange {
    fn new(start: Pc, end: Pc) -> BBRange {
        assert!(start <= end);
        BBRange { start, end }
    }

    fn contains(&self, pc: Pc) -> bool {
        self.start <= pc && self.end >= pc
    }

    fn intersects(&self, other: &BBRange) -> bool {
        self.start <= other.end && other.start <= self.end
    }
}

struct SubroutineBuilder<'a> {
    rom: Rom<'a>,
    addr: Pc,
    has_ret: bool,
    bbs: BasicBlocksBuilder,
    bb_ranges: HashMap<BasicBlockId, BBRange>,
}

impl<'a> SubroutineBuilder<'a> {
    fn new(rom: Rom<'a>, addr: Pc) -> SubroutineBuilder<'a> {
        SubroutineBuilder {
            rom,
            addr,
            has_ret: false,
            bbs: BasicBlocksBuilder::new(),
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

    fn build_bb(
        &mut self,
        pc: &mut Pc,
        instructions: &mut Vec<Instruction>,
        leaders: &HashMap<Pc, BasicBlockId>,
    ) -> Result<Terminator> {
        let terminator = loop {
            let instruction = self.rom.decode_instruction(*pc)?;
            println!("pc={:?}, {:?}", pc, instruction);

            // Check, if current instruction is a terminator.
            match instruction {
                Instruction::Ret => {
                    break Terminator::Ret;
                }
                Instruction::Jump(addr) => {
                    let target_pc = Pc::from(addr);
                    let target_id = leaders[&target_pc];
                    break Terminator::Jump { target: target_id };
                }
                Instruction::Skip(predicate) => {
                    let next_pc = pc.advance(1);
                    let skip_pc = pc.advance(2);
                    let next_id = leaders[&next_pc];
                    let skip_id = leaders[&skip_pc];
                    break Terminator::Skip {
                        predicate,
                        next: next_id,
                        skip: skip_id,
                    };
                }
                _ => {}
            }

            // Otherwise, put current instructions into result vector.
            instructions.push(instruction);

            // If next instruction is a leader, then this instruction is last in current
            // basic block.
            if let Some(next_block_id) = leaders.get(&pc.advance(1)) {
                break Terminator::Jump {
                    target: *next_block_id,
                };
            }
            *pc = pc.advance(1);
        };

        Ok(terminator)
    }

    fn identify_leaders(
        &mut self,
        seen_calls: &mut HashSet<Addr>,
    ) -> Result<HashMap<Pc, BasicBlockId>> {
        let mut leaders: HashMap<Pc, BasicBlockId> = HashMap::new();

        let mut stack: Vec<Pc> = Vec::new();
        stack.push(self.addr);
        leaders.insert(self.addr, BasicBlockId::entry());

        while let Some(mut pc) = stack.pop() {
            loop {
                let instruction = self.rom.decode_instruction(pc)?;
                match instruction {
                    Instruction::Ret => {
                        self.has_ret = true;
                        break;
                    }
                    Instruction::Jump(addr) => {
                        let target_pc = Pc::from(addr);
                        if !leaders.contains_key(&target_pc) {
                            leaders.insert(target_pc, self.bbs.gen_id());
                            stack.push(target_pc);
                        }
                        break;
                    }
                    Instruction::Skip(_) => {
                        let next_pc = pc.advance(1);
                        let skip_pc = pc.advance(2);

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
                pc = pc.advance(1);
            }
        }

        Ok(leaders)
    }

    fn build_cfg(&mut self, seen_calls: &mut HashSet<Addr>) -> Result<()> {
        let leaders = self.identify_leaders(seen_calls)?;

        println!("leaders = {:?}", leaders);

        for leader_pc in leaders.keys().cloned() {
            let mut pc = leader_pc;
            let mut instructions = Vec::new();

            let terminator = self.build_bb(&mut pc, &mut instructions, &leaders)?;

            let bb_id = leaders[&leader_pc];
            self.seal_bb(bb_id, BBRange::new(leader_pc, pc), instructions, terminator);
        }

        Ok(())
    }
}

pub fn build_cfg(rom: &[u8]) -> Result<CFG> {
    let mut seen_calls = HashSet::new();
    let mut subs = HashMap::new();
    let mut subroutine_stack: Vec<Addr> = Vec::new();

    let start_subroutine_id = RoutineId::start().0;
    subroutine_stack.push(start_subroutine_id);

    while let Some(subroutine_addr) = subroutine_stack.pop() {
        let mut sub_builder = SubroutineBuilder::new(Rom::new(rom), Pc::from(subroutine_addr));
        let mut seen_calls_from_sr = HashSet::new();
        sub_builder.build_cfg(&mut seen_calls_from_sr)?;
        if RoutineId(subroutine_addr) == RoutineId::start() {
            // TODO: convert to Err
            assert!(!sub_builder.has_ret);
        }
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

impl RoutineId {
    pub fn start() -> RoutineId {
        RoutineId(Addr(ROM_START_ADDR as u16))
    }
}

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
struct BasicBlocksBuilder {
    id_counter: usize,
    bbs: HashMap<BasicBlockId, BasicBlock>,
}

impl BasicBlocksBuilder {
    fn new() -> BasicBlocksBuilder {
        BasicBlocksBuilder {
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

impl From<HashMap<BasicBlockId, BasicBlock>> for BasicBlocksBuilder {
    fn from(map: HashMap<BasicBlockId, BasicBlock>) -> BasicBlocksBuilder {
        let max_id = map.keys().map(|b| b.0).max().unwrap_or(0);
        BasicBlocksBuilder {
            id_counter: max_id + 1,
            bbs: map,
        }
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

#[test]
fn test_bbrange_contains() {
    let bb = BBRange::new(Pc(0), Pc(2));
    assert!(bb.contains(Pc(0)));
    assert!(bb.contains(Pc(1)));
    assert!(bb.contains(Pc(2)));
    assert!(!bb.contains(Pc(3)));
}

#[test]
fn test_bbrange_intersects() {
    fn intersects(a: (usize, usize), b: (usize, usize)) -> bool {
        BBRange::new(Pc(a.0), Pc(a.1)).intersects(&BBRange::new(Pc(b.0), Pc(b.1)))
    }

    assert!(intersects((0, 3), (1, 4))); // intersection 1-3
    assert!(intersects((0, 3), (0, 1))); // intersection 0-1
    assert!(intersects((0, 0), (0, 0))); // intersection 0-0
    assert!(intersects((5, 10), (6, 7))); // intersection 6-7

    assert!(!intersects((0, 5), (6, 10))); // intersection 6-7
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

    let entry_id = start_routine.entry;
    let entry_bb = &start_routine.bbs[&entry_id];

    assert_eq!(entry_bb.insts, vec![]);

    let (next_id, skip_id) = match entry_bb.terminator {
        Terminator::Skip { next, skip, .. } => (next, skip),
        unexpected => panic!("expected Skip, but found {:?}", unexpected),
    };

    let next_jmp_id = match start_routine.bbs[&next_id].terminator {
        Terminator::Jump { target } => target,
        unexpected => panic!("expected Jump, but found {:?}", unexpected),
    };
    let skip_jmp_id = match start_routine.bbs[&skip_id].terminator {
        Terminator::Jump { target } => target,
        unexpected => panic!("expected Jump, but found {:?}", unexpected),
    };

    assert_eq!(
        start_routine.bbs[&next_jmp_id].insts,
        vec![Instruction::Sys(Addr(0x002))]
    );
    assert_eq!(
        start_routine.bbs[&next_jmp_id].terminator,
        Terminator::Jump { target: entry_id }
    );
    assert_eq!(
        start_routine.bbs[&skip_jmp_id].insts,
        vec![Instruction::Sys(Addr(0x001))]
    );
    assert_eq!(
        start_routine.bbs[&skip_jmp_id].terminator,
        Terminator::Jump {
            target: next_jmp_id,
        }
    );
}
