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
}

impl Drop for TransCtx {
    fn drop(&mut self) {
        unsafe { ffi::BinaryenModuleDispose(self.module) }
    }
}

fn trans(cfg: cfg::CFG) {
    let module = unsafe { ffi::BinaryenModuleCreate() };
    let ctx = TransCtx { module, cfg };

    let start = ctx.cfg.start();

    let mut routine_ctx = RoutineTransCtx::new(&ctx, start);
    routine_ctx.trans();
}

struct RoutineTransCtx<'a, 't> {
    ctx: &'t TransCtx,
    routine: &'a cfg::Routine,
    relooper: ffi::RelooperRef,
}

impl<'a, 't> RoutineTransCtx<'a, 't> {
    fn new(ctx: &'t TransCtx, routine: &'a cfg::Routine) -> RoutineTransCtx<'a, 't> {
        let relooper = unsafe { ffi::RelooperCreate() };
        RoutineTransCtx {
            ctx,
            routine,
            relooper,
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
    }

    fn trans_bb(&mut self, bb_id: cfg::BasicBlockId) -> ffi::RelooperBlockRef {
        let bb = &self.routine.bbs[&bb_id];

        let mut stmts = Vec::new();

        for inst in bb.instructions() {
            unsafe { self.trans_instruction(inst, &mut stmts) };
        }

        if let cfg::Terminator::Ret = bb.terminator() {
            unsafe {
                stmts.push(ffi::BinaryenReturn(self.ctx.module, ptr::null_mut()));
            }
        }

        let relooper_block = unsafe {
            let code = ffi::BinaryenBlock(
                self.ctx.module,
                ptr::null_mut(),
                stmts.as_ptr() as _,
                stmts.len() as _,
                ffi::BinaryenNone(),
            );
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
                let imm_expr = self.load_const_i32(imm.0 as u32);
                stmts.push(self.store_reg(vx, imm_expr));
            }

            _ => panic!(),
        }
    }

    fn trans_cond(&mut self, cond: Cond) -> ffi::BinaryenExpressionRef {
        let lhs = self.load_reg(cond.vx);
        let rhs = match cond.rhs {
            CondRhs::Reg(vy) => self.load_reg(vy),
            CondRhs::Imm(imm) => self.load_const_i32(imm.0 as u32),
            CondRhs::Pressed => panic!("TODO"),
        };

        unsafe { ffi::BinaryenBinary(self.ctx.module, ffi::BinaryenEqInt32(), lhs, rhs) }
    }

    fn load_reg(&mut self, reg: Reg) -> ffi::BinaryenExpressionRef {
        unsafe {
            let reg_ptr: u32 = reg.index().into();
            ffi::BinaryenLoad(
                self.ctx.module,
                1,
                0,
                0,
                0,
                ffi::BinaryenInt32(),
                self.load_const_i32(reg_ptr),
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
                self.ctx.module,
                1,
                0,
                0,
                self.load_const_i32(reg_ptr),
                value,
                ffi::BinaryenInt32(),
            )
        }
    }

    fn load_const_i32(&mut self, c: u32) -> ffi::BinaryenExpressionRef {
        unsafe { ffi::BinaryenConst(self.ctx.module, ffi::BinaryenLiteralInt32(c as i32)) }
    }
}
