use super::{Result, ErrorKind};

#[derive(Debug, Copy, Clone)]
pub struct InstructionWord(pub u16);

impl InstructionWord {
    pub fn op(self) -> u8 {
        ((self.0 & 0xF000) >> 12) as u8
    }

    pub fn nnn(self) -> u16 {
        self.0 & 0x0FFF
    }

    pub fn addr(self) -> Addr {
        Addr(self.nnn())
    }

    pub fn kk(self) -> u8 {
        (self.0 & 0xFF) as u8
    }

    pub fn imm(self) -> Imm {
        Imm(self.kk())
    }

    pub fn n(self) -> u8 {
        (self.0 & 0xF) as u8
    }

    pub fn imm4(self) -> Imm4 {
        Imm4(self.n())
    }

    pub fn x_reg(self) -> Reg {
        let index = ((self.0 & 0x0F00) >> 8) as u8;
        Reg::from_index(index)
    }

    pub fn y_reg(self) -> Reg {
        let index = ((self.0 & 0x00F0) >> 4) as u8;
        Reg::from_index(index)
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct Addr(pub u16); // TODO: Only & 0x0FFF

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Imm(pub u8);

impl Imm {
    fn encode_as_kk(self) -> u16 {
        self.0 as u16
    }
}

#[derive(Debug, PartialEq)]
pub struct Imm4(pub u8); // TODO: Only & 0x0F

impl Imm4 {
    fn encode_as_n(self) -> u16 {
        self.0 as u16
    }
}

enum_from_primitive! {
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Reg {
    V0 = 0x0,
    V1 = 0x1,
    V2 = 0x2,
    V3 = 0x3,
    V4 = 0x4,
    V5 = 0x5,
    V6 = 0x6,
    V7 = 0x7,
    V8 = 0x8,
    V9 = 0x9,
    Va = 0xA,
    Vb = 0xB,
    Vc = 0xC,
    Vd = 0xD,
    Ve = 0xE,
    Vf = 0xF
}
}

impl Reg {
    pub fn from_index(index: u8) -> Reg {
        use enum_primitive::FromPrimitive;
        Reg::from_u8(index).expect("index should be between [0x00, 0x0F]")
    }

    pub fn index(self) -> u8 {
        self as u8
    }

    fn encode_as_vx(self) -> u16 {
        (self as u16) << 8
    }

    fn encode_as_vy(self) -> u16 {
        (self as u16) << 4
    }
}

enum_from_primitive! {
#[derive(Debug, PartialEq)]
pub enum Fun {
    // 8xy0 - LD Vx, Vy
    Id = 0x0,
    
    // 8xy1 - OR Vx, Vy 
    Or = 0x1,
    
    // 8xy2 - AND Vx, Vy
    And = 0x2,
    
    // 8xy3 - XOR Vx, Vy
    Xor = 0x3,
    
    // 8xy4 - ADD Vx, Vy
    Add = 0x4,
    
    // 8xy5 - SUB Vx, Vy
    Subtract = 0x5, 
    
    // 8xy6 - SHR Vx {, Vy}
    ShiftRight = 0x6,
    
    // 8xy7 - SUBN Vx, Vy
    SubtractInv = 0x7,
    
    // 8xyE - SHL Vx {, Vy}
    ShiftLeft = 0xE
}
}

#[derive(Debug, PartialEq)]
pub enum Instruction {
    /// 00E0 - CLS
    ClearScreen,

    /// 00EE - RET
    Ret,

    /// 0nnn - SYS addr
    Sys(Addr),

    /// 1nnn - JP addr
    Jump(Addr),

    // 2nnn - CALL addr
    Call(Addr),

    /// 3xkk - SE Vx, byte
    /// 4xkk - SNE Vx, byte
    SkipEqImm { vx: Reg, imm: Imm, inv: bool },

    /// 5xy0 - SE Vx, Vy
    /// 9xy0 - SNE Vx, Vy
    SkipEqReg { vx: Reg, vy: Reg, inv: bool },

    /// 6xkk - LD Vx, byte
    PutImm { vx: Reg, imm: Imm },

    /// 7xkk - ADD Vx, byte
    AddImm { vx: Reg, imm: Imm },

    /// 8xyF, where F is function
    Apply { vx: Reg, vy: Reg, f: Fun },

    /// Annn - LD I, addr
    SetI(Addr),

    /// Bnnn - JP V0, addr
    JumpPlusV0(Addr),

    /// Cxkk - RND Vx, byte
    Randomize { vx: Reg, imm: Imm },

    /// Dxyn - DRW Vx, Vy, nibble
    Draw { vx: Reg, vy: Reg, n: Imm4 },

    /// Ex9E - SKP Vx
    /// ExA1 - SKNP Vx
    SkipPressed { vx: Reg, inv: bool },

    /// Fx07 - LD Vx, DT
    GetDT(Reg),

    /// Fx0A - LD Vx, K
    WaitKey(Reg),

    /// Fx15 - LD DT, Vx
    SetDT(Reg),

    // Fx18 - LD ST, Vx
    SetST(Reg),

    // Fx1E - ADD I, Vx
    AddI(Reg),

    // Fx29 - LD F, Vx
    LoadGlyph(Reg),

    /// Fx33 - LD B, Vx
    StoreBCD(Reg),

    /// Fx55 - LD [I], Vx
    StoreRegs(Reg),

    /// Fx65 - LD Vx, [I]
    LoadRegs(Reg),
}

impl Instruction {
    pub fn decode(iw: InstructionWord) -> Result<Instruction> {
        use self::Instruction::*;

        let insn = match iw.op() {
            0x0 => {
                match iw.kk() {
                    0xE0 => ClearScreen,
                    0xEE => Ret,
                    _ => Sys(iw.addr()),
                }
            }
            0x1 => Jump(iw.addr()),
            0x2 => Call(iw.addr()),
            0x3 => {
                SkipEqImm {
                    vx: iw.x_reg(),
                    imm: iw.imm(),
                    inv: false,
                }
            }
            0x4 => {
                SkipEqImm {
                    vx: iw.x_reg(),
                    imm: iw.imm(),
                    inv: true,
                }
            }
            0x5 => {
                SkipEqReg {
                    vx: iw.x_reg(),
                    vy: iw.y_reg(),
                    inv: false,
                }
            }
            0x9 => {
                SkipEqReg {
                    vx: iw.x_reg(),
                    vy: iw.y_reg(),
                    inv: true,
                }
            }
            0x6 => {
                PutImm {
                    vx: iw.x_reg(),
                    imm: iw.imm(),
                }
            }
            0x7 => {
                AddImm {
                    vx: iw.x_reg(),
                    imm: iw.imm(),
                }
            }
            0x8 => {
                Apply {
                    vx: iw.x_reg(),
                    vy: iw.y_reg(),
                    f: {
                        use enum_primitive::FromPrimitive;

                        Fun::from_u8(iw.n()).ok_or(
                            ErrorKind::UnrecognizedInstruction(iw),
                        )?
                    },
                }
            }
            0xA => SetI(iw.addr()),
            0xB => JumpPlusV0(iw.addr()),
            0xC => {
                Randomize {
                    vx: iw.x_reg(),
                    imm: iw.imm(),
                }
            }
            0xD => {
                Draw {
                    vx: iw.x_reg(),
                    vy: iw.y_reg(),
                    n: iw.imm4(),
                }
            }
            0xE => {
                match iw.kk() {
                    0x9E => {
                        SkipPressed {
                            vx: iw.x_reg(),
                            inv: false,
                        }
                    }
                    0xA1 => {
                        SkipPressed {
                            vx: iw.x_reg(),
                            inv: true,
                        }
                    }
                    _ => bail!(ErrorKind::UnrecognizedInstruction(iw)),
                }
            }
            0xF => {
                match iw.kk() {
                    0x07 => GetDT(iw.x_reg()),
                    0x0A => WaitKey(iw.x_reg()),
                    0x15 => SetDT(iw.x_reg()),
                    0x18 => SetST(iw.x_reg()),
                    0x1E => AddI(iw.x_reg()),
                    0x29 => LoadGlyph(iw.x_reg()),
                    0x33 => StoreBCD(iw.x_reg()),
                    0x55 => StoreRegs(iw.x_reg()),
                    0x65 => LoadRegs(iw.x_reg()),
                    _ => bail!(ErrorKind::UnrecognizedInstruction(iw)),
                }
            }
            _ => bail!(ErrorKind::UnrecognizedInstruction(iw)),
        };
        Ok(insn)
    }

    pub fn encode(self) -> InstructionWord {
        use self::Instruction::*;

        let encoding: u16 = match self {
            ClearScreen => 0x00E0,
            Ret => 0x00EE,
            Jump(addr) => 0x1000 | addr.0,
            Call(addr) => 0x2000 | addr.0,
            SkipEqImm { vx, imm, inv } => {
                let opcode = if !inv { 0x3000 } else { 0x4000 };
                opcode | vx.encode_as_vx() | imm.encode_as_kk()
            }
            SkipEqReg { vx, vy, inv } => {
                let opcode = if !inv { 0x5000 } else { 0x9000 };
                opcode | vx.encode_as_vx() | vy.encode_as_vy()
            }
            PutImm { vx, imm } => 0x6000 | vx.encode_as_vx() | imm.encode_as_kk(),
            AddImm { vx, imm } => 0x7000 | vx.encode_as_vx() | imm.encode_as_kk(),
            Apply { vx, vy, f } => {
                let fun_op = f as u16;
                0x8000 | vx.encode_as_vx() | vy.encode_as_vy() | fun_op
            }
            SetI(addr) => 0xA000 | addr.0,
            JumpPlusV0(addr) => 0xB000 | addr.0,
            Randomize { vx, imm } => 0xC000 | vx.encode_as_vx() | imm.encode_as_kk(),
            Draw { vx, vy, n } => 0xD000 | vx.encode_as_vx() | vy.encode_as_vy() | n.encode_as_n(),
            SkipPressed { vx, inv } => {
                let sub_op = if !inv { 0x009E } else { 0x00A1 };
                0xE000 | vx.encode_as_vx() | sub_op
            }
            GetDT(vx) => 0xF000 | vx.encode_as_vx() | 0x0007,
            WaitKey(vx) => 0xF000 | vx.encode_as_vx() | 0x000A,
            SetDT(vx) => 0xF000 | vx.encode_as_vx() | 0x0015,
            SetST(vx) => 0xF000 | vx.encode_as_vx() | 0x0018,
            AddI(vx) => 0xF000 | vx.encode_as_vx() | 0x001E,
            LoadGlyph(vx) => 0xF000 | vx.encode_as_vx() | 0x0029,
            StoreBCD(vx) => 0xF000 | vx.encode_as_vx() | 0x0033,
            StoreRegs(vx) => 0xF000 | vx.encode_as_vx() | 0x0055,
            LoadRegs(vx) => 0xF000 | vx.encode_as_vx() | 0x0065,
            _ => unimplemented!(),  
        };
        InstructionWord(encoding)
    }
}

#[test]
fn test_apply_xor() {
    let instruction = Instruction::Apply {
        vx: Reg::V3,
        vy: Reg::Vf,
        f: Fun::Xor,
    };
    assert_eq!(instruction.encode().0, 0x83F3);
}
