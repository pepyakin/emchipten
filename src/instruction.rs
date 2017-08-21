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

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
pub struct Addr(pub u16); // TODO: Only & 0x0FFF

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Imm(pub u8);

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Imm4(pub u8); // TODO: Only & 0x0F

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
}

enum_from_primitive! {
#[derive(Copy, Clone, Debug, PartialEq)]
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

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Cmp {
    Eq,
    Ne,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum CondRhs {
    Imm(Imm),
    Reg(Reg),
    Pressed,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Cond {
    pub vx: Reg,
    pub cmp: Cmp,
    pub rhs: CondRhs,
}

#[derive(Copy, Clone, Debug, PartialEq)]
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
    /// 5xy0 - SE Vx, Vy
    /// 9xy0 - SNE Vx, Vy
    /// Ex9E - SKP Vx
    /// ExA1 - SKNP Vx
    Skip(Cond),

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
                Skip(Cond {
                    vx: iw.x_reg(),
                    cmp: Cmp::Eq,
                    rhs: CondRhs::Imm(iw.imm()),
                })
            }
            0x4 => {
                Skip(Cond {
                    vx: iw.x_reg(),
                    cmp: Cmp::Ne,
                    rhs: CondRhs::Imm(iw.imm()),
                })
            }
            0x5 => {
                Skip(Cond {
                    vx: iw.x_reg(),
                    cmp: Cmp::Eq,
                    rhs: CondRhs::Reg(iw.y_reg()),
                })
            }
            0x9 => {
                Skip(Cond {
                    vx: iw.x_reg(),
                    cmp: Cmp::Ne,
                    rhs: CondRhs::Reg(iw.y_reg()),
                })
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
                        Skip(Cond {
                            vx: iw.x_reg(),
                            cmp: Cmp::Eq,
                            rhs: CondRhs::Pressed,
                        })
                    }
                    0xA1 => {
                        Skip(Cond {
                            vx: iw.x_reg(),
                            cmp: Cmp::Ne,
                            rhs: CondRhs::Pressed,
                        })
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
