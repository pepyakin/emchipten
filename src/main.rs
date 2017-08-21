#![feature(inclusive_range_syntax)]

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
use std::ptr;
use std::path::Path;
use instruction::*;
use binaryen::ffi;
use std::ffi::CString;

use std::collections::HashMap;

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
    let module = cfg::build_cfg(&rom_buffer).unwrap();
    println!("{:#?}", module);
    module.print();

    trans(module);
}

struct TransCtx {
    module: ffi::BinaryenModuleRef,
    cfg: cfg::CFG,
    c_strings: Vec<CString>,
}

impl Drop for TransCtx {
    fn drop(&mut self) {
        unsafe { ffi::BinaryenModuleDispose(self.module) }
    }
}

fn trans(cfg: cfg::CFG) {
    let module = unsafe { ffi::BinaryenModuleCreate() };
    let mut ctx = TransCtx { 
        module, 
        cfg, 
        c_strings: Vec::new() 
    };

    let mut routine_ctx = RoutineTransCtx::new(&mut ctx);
    routine_ctx.trans();
}

struct RoutineTransCtx<'t> {
    module: ffi::BinaryenModuleRef,
    routine: &'t cfg::Routine,
    relooper: ffi::RelooperRef,
    c_strings: &'t mut Vec<CString>
}

impl<'t> RoutineTransCtx<'t> {
    fn new(ctx: &'t mut TransCtx) -> RoutineTransCtx<'t> {
        let routine = ctx.cfg.start();
        let relooper = unsafe { ffi::RelooperCreate() };
        RoutineTransCtx {
            module: ctx.module,
            routine,
            relooper,
            c_strings: &mut ctx.c_strings,
        }
    }

    fn trans(&mut self) {
        let mut relooper_blocks = HashMap::new();
        for bb_id in self.routine.bbs.keys() {
            let relooper_block = self.trans_bb(*bb_id);
            relooper_blocks.insert(*bb_id, relooper_block);
        }

        for bb_id in self.routine.bbs.keys() {
            let bb = &self.routine.bbs[&bb_id];

            let from_relooper_block = relooper_blocks[&bb_id];

            use cfg::Terminator::*;
            match bb.terminator() {
                Ret => {
                    // Return should be added in trans_bb()
                }
                Jump { target } => unsafe {
                    let to_relooper_block = relooper_blocks[&target];
                    ffi::RelooperAddBranch(
                        from_relooper_block,
                        to_relooper_block,
                        ptr::null_mut(),
                        ptr::null_mut(),
                    );
                },
                Skip { cond, next, skip } => unsafe {
                    let cond = self.trans_cond(cond);
                    let skip_relooper_block = relooper_blocks[&skip];
                    let next_relooper_block = relooper_blocks[&next];

                    ffi::RelooperAddBranch(
                        from_relooper_block,
                        skip_relooper_block,
                        cond,
                        ptr::null_mut(),
                    );
                    ffi::RelooperAddBranch(
                        from_relooper_block,
                        next_relooper_block,
                        ptr::null_mut(),
                        ptr::null_mut(),
                    );
                },
            }
        }

        let relooper_entry_block = relooper_blocks[&self.routine.entry];

        unsafe {
            // TODO: 0??
            let body_code = ffi::RelooperRenderAndDispose(self.relooper, relooper_entry_block, 0, self.module);
            // ffi::BinaryenExpressionPrint(body_code);
        }
    }

    fn trans_bb(&mut self, bb_id: cfg::BasicBlockId) -> ffi::RelooperBlockRef {
        let bb = &self.routine.bbs[&bb_id];

        println!("{:#?}", bb);

        let mut stmts = Vec::new();

        for inst in bb.instructions() {
            unsafe { self.trans_instruction(inst, &mut stmts) };
        }

        if let cfg::Terminator::Ret = bb.terminator() {
            unsafe {
                stmts.push(ffi::BinaryenReturn(self.module, ptr::null_mut()));
            }
        }

        let relooper_block = unsafe {
            let code = ffi::BinaryenBlock(
                self.module,
                ptr::null_mut(),
                stmts.as_ptr() as _,
                stmts.len() as _,
                ffi::BinaryenNone(),
            );
            ffi::BinaryenExpressionPrint(code);
            ffi::RelooperAddBlock(self.relooper, code)
        };
        relooper_block
    }

    unsafe fn trans_instruction(
        &mut self,
        instruction: &Instruction,
        stmts: &mut Vec<ffi::BinaryenExpressionRef>,
    ) {
        match *instruction {
            Instruction::PutImm { vx, imm } => {
                let imm_expr = self.load_imm(imm.0 as u32);
                stmts.push(self.store_reg(vx, imm_expr));
            }
            Instruction::AddImm { vx, imm } => {
                let imm_expr = self.load_imm(imm.0 as u32);
                let load_expr = self.load_reg(vx);
                let add_expr = ffi::BinaryenBinary(self.module, ffi::BinaryenAddInt32(), load_expr, imm_expr);
                stmts.push(self.store_reg(vx, add_expr));
            }
            Instruction::Randomize { vx, imm } => {
                // TODO: Optimize
                let random = CString::new("random").unwrap();
                let random_name = random.as_ptr();
                self.c_strings.push(random);

                let rnd_expr = ffi::BinaryenCallImport(self.module, random_name, ptr::null_mut(), 0, ffi::BinaryenInt32());
                let mask_imm_expr = self.load_imm(imm.0 as u32);
                let mask_expr = ffi::BinaryenBinary(self.module, ffi::BinaryenAndInt32(), rnd_expr, mask_imm_expr);
                let store_expr = self.store_reg(vx, mask_expr);
                stmts.push(store_expr);
            }
            Instruction::Draw { vx, vy, n } => {
                // TODO: Optimize
                let draw = CString::new("draw").unwrap();
                let draw_name = draw.as_ptr();
                self.c_strings.push(draw);

                let x_expr = self.load_reg(vx);
                let y_expr = self.load_reg(vy);
                let n_expr = self.load_imm(n.0 as u32);

                let operands = vec![x_expr, y_expr, n_expr];

                let draw_expr = ffi::BinaryenCallImport(self.module, draw_name, operands.as_ptr() as _, operands.len() as _, ffi::BinaryenInt32());
                let store_expr = self.store_reg(Reg::Vf, draw_expr);

                stmts.push(store_expr);
            }
            Instruction::SetI(addr) => {
                let store_i_expr = self.store_i_imm(addr);
                stmts.push(store_i_expr);
            }
            _ => panic!("unimplemented"),
        }
    }

    fn trans_cond(&mut self, cond: Cond) -> ffi::BinaryenExpressionRef {
        let lhs = self.load_reg(cond.vx);
        let rhs = match cond.rhs {
            CondRhs::Reg(vy) => self.load_reg(vy),
            CondRhs::Imm(imm) => self.load_imm(imm.0 as u32),
            CondRhs::Pressed => panic!("TODO"),
        };

        unsafe { ffi::BinaryenBinary(self.module, ffi::BinaryenEqInt32(), lhs, rhs) }
    }

    fn store_i_imm(&mut self, addr: Addr) -> ffi::BinaryenExpressionRef {
        let i_ptr: u32 = 0x10;
        unsafe {
            ffi::BinaryenStore(
                self.module,
                2,
                0,
                0,
                self.load_imm(i_ptr),
                self.load_imm(addr.0 as u32),
                ffi::BinaryenInt32(),
            )   
        }
    }

    fn load_reg(&mut self, reg: Reg) -> ffi::BinaryenExpressionRef {
        unsafe {
            let reg_ptr: u32 = reg.index().into();
            ffi::BinaryenLoad(
                self.module,
                1,
                0,
                0,
                0,
                ffi::BinaryenInt32(),
                self.load_imm(reg_ptr),
            )
        }
    }

    fn store_reg(
        &mut self,
        reg: Reg,
        value: ffi::BinaryenExpressionRef,
    ) -> ffi::BinaryenExpressionRef {
        unsafe {
            let reg_ptr: u32 = reg.index().into();
            ffi::BinaryenStore(
                self.module,
                1,
                0,
                0,
                self.load_imm(reg_ptr),
                value,
                ffi::BinaryenInt32(),
            )
        }
    }

    fn load_imm(&mut self, c: u32) -> ffi::BinaryenExpressionRef {
        unsafe { ffi::BinaryenConst(self.module, ffi::BinaryenLiteralInt32(c as i32)) }
    }
}
