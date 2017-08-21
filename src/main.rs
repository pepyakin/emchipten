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
        c_strings: Vec::new(),
    };

    let mut routine_ctx = RoutineTransCtx::new(&mut ctx);
    routine_ctx.trans();
}

struct RoutineTransCtx<'t> {
    module: ffi::BinaryenModuleRef,
    routine: &'t cfg::Routine,
    relooper: ffi::RelooperRef,
    c_strings: &'t mut Vec<CString>,
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
            let body_code =
                ffi::RelooperRenderAndDispose(self.relooper, relooper_entry_block, 0, self.module);
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
                let add_expr =
                    ffi::BinaryenBinary(self.module, ffi::BinaryenAddInt32(), load_expr, imm_expr);
                stmts.push(self.store_reg(vx, add_expr));
            }
            Instruction::Randomize { vx, imm } => {
                let rnd_expr = self.trans_call_import("random", vec![], ffi::BinaryenInt32());
                let mask_imm_expr = self.load_imm(imm.0 as u32);
                let mask_expr = ffi::BinaryenBinary(
                    self.module,
                    ffi::BinaryenAndInt32(),
                    rnd_expr,
                    mask_imm_expr,
                );
                let store_expr = self.store_reg(vx, mask_expr);
                stmts.push(store_expr);
            }
            Instruction::Draw { vx, vy, n } => {
                let x_expr = self.load_reg(vx);
                let y_expr = self.load_reg(vy);
                let n_expr = self.load_imm(n.0 as u32);

                let operands = vec![x_expr, y_expr, n_expr];
                let draw_expr = self.trans_call_import("draw", operands, ffi::BinaryenInt32());
                let store_expr = self.store_reg(Reg::Vf, draw_expr);

                stmts.push(store_expr);
            }
            Instruction::SetI(addr) => {
                let imm_expr = self.load_imm(addr.0 as u32);
                let store_i_expr = self.store_i(imm_expr);
                stmts.push(store_i_expr);
            }
            Instruction::AddI(vx) => {
                let vx_expr = self.load_reg(vx);
                let load_i_expr = self.load_i();
                let add_expr = ffi::BinaryenBinary(self.module, ffi::BinaryenAddInt32(), vx_expr, load_i_expr);
                let store_i_expr = self.store_i(add_expr);
                // TODO: Wrapping
                stmts.push(store_i_expr);
            }
            Instruction::GetDT(vx) => {
                let get_dt_expr = self.trans_call_import("get_dt", vec![], ffi::BinaryenInt32());
                let store_expr = self.store_reg(vx, get_dt_expr);
                stmts.push(store_expr);
            }
            Instruction::SetST(vx) => {
                let load_expr = self.load_reg(vx);
                let set_st_expr = self.trans_call_import("set_st", vec![load_expr], ffi::BinaryenNone());
                // TODO: Drop?
                stmts.push(set_st_expr);
            }
            Instruction::SetDT(vx) => {
                let load_expr = self.load_reg(vx);
                let set_dt_expr = self.trans_call_import("set_dt", vec![load_expr], ffi::BinaryenNone());
                // TODO: Drop?
                stmts.push(set_dt_expr);
            }
            Instruction::WaitKey(vx) => {
                let wait_key_expr = self.trans_call_import("wait_key", vec![], ffi::BinaryenInt32());
                let store_expr = self.store_reg(vx, wait_key_expr);
                stmts.push(store_expr);
            }
            Instruction::LoadRegs(vx) => {
                for offset in 0..(vx.index() as usize + 1) {
                    let reg = Reg::from_index(offset as u8);
                    let load_mem_expr = self.load_mem_at_i();
                    let store_expr = self.store_reg(reg, load_mem_expr);
                    stmts.push(store_expr);

                    let imm_1_expr = self.load_imm(1);
                    let load_i_expr = self.load_i();
                    let increment_i_expr = ffi::BinaryenBinary(self.module, ffi::BinaryenAddInt32(), imm_1_expr, load_i_expr);
                    let store_i_expr = self.store_i(increment_i_expr);

                    stmts.push(store_i_expr);
                }
            }
            Instruction::StoreRegs(vx) => {
                for offset in 0..(vx.index() as usize + 1) {
                    let reg = Reg::from_index(offset as u8);
                    let load_expr = self.load_reg(reg);
                    let store_mem_expr = self.store_mem_at_i(load_expr);
                    stmts.push(store_mem_expr);

                    let imm_1_expr = self.load_imm(1);
                    let load_i_expr = self.load_i();
                    let increment_i_expr = ffi::BinaryenBinary(self.module, ffi::BinaryenAddInt32(), imm_1_expr, load_i_expr);
                    let store_i_expr = self.store_i(increment_i_expr);

                    stmts.push(store_i_expr);
                }
            }
            Instruction::LoadGlyph(vx) => {
                // (vx & 0xf) << 3
                let vx_expr = self.load_reg(vx);
                let mask_imm_expr = self.load_imm(0x0f);
                let mask_expr = ffi::BinaryenBinary(
                    self.module,
                    ffi::BinaryenAndInt32(),
                    vx_expr,
                    mask_imm_expr,
                );
                let shift_imm_expr = self.load_imm(3);
                let shift_expr = ffi::BinaryenBinary(
                    self.module,
                    ffi::BinaryenShlInt32(),
                    mask_expr,
                    shift_imm_expr,
                );
                let store_i_expr = self.store_i(shift_expr);

                stmts.push(store_i_expr);
            }
            _ => panic!("unimplemented: {:#?}", instruction),
        }
    }

    fn trans_call_import(&mut self, name: &str, operands: Vec<ffi::BinaryenExpressionRef>, result_ty: ffi::BinaryenType) -> ffi::BinaryenExpressionRef {
        // TODO: Optimize
        unsafe {
            let fn_name = CString::new(name).unwrap();
            let fn_name_ptr = fn_name.as_ptr();
            self.c_strings.push(fn_name);

            ffi::BinaryenCallImport(
                self.module,
                fn_name_ptr,
                operands.as_ptr() as _,
                operands.len() as _,
                result_ty,
            )
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

    fn load_i(&mut self) -> ffi::BinaryenExpressionRef {
        let i_ptr: u32 = 0x10;
        unsafe {
            ffi::BinaryenLoad(
                self.module,
                2,
                0,
                0,
                0,
                ffi::BinaryenInt32(),
                self.load_imm(i_ptr),
            )
        }
    }

    fn store_i(&mut self, value: ffi::BinaryenExpressionRef) -> ffi::BinaryenExpressionRef {
        let i_ptr: u32 = 0x10;
        unsafe {
            ffi::BinaryenStore(
                self.module,
                2,
                0,
                0,
                self.load_imm(i_ptr),
                value,
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

    fn load_mem_at_i(&mut self) -> ffi::BinaryenExpressionRef {
        let i_expr = self.load_i();
        unsafe {
            ffi::BinaryenLoad(
                self.module,
                1,
                0,
                0,
                0,
                ffi::BinaryenInt32(),
                i_expr,
            )
        }
    }

    fn store_mem_at_i(&mut self, value: ffi::BinaryenExpressionRef) -> ffi::BinaryenExpressionRef {
        let i_expr = self.load_i();
        unsafe {
            ffi::BinaryenStore(
                self.module,
                1,
                0,
                0,
                i_expr,
                value,
                ffi::BinaryenInt32(),
            )
        }
    }
}
