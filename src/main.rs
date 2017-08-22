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
    let cfg = cfg::build_cfg(&rom_buffer).unwrap();
    println!("{:#?}", cfg);
    cfg.print();

    trans(&cfg);
}

struct TransCtx<'a> {
    module: ffi::BinaryenModuleRef,
    cfg: &'a cfg::CFG,
    c_strings: Vec<CString>,
    procedure_fn_ty: ffi::BinaryenFunctionTypeRef,
}

impl<'a> Drop for TransCtx<'a> {
    fn drop(&mut self) {
        unsafe { ffi::BinaryenModuleDispose(self.module) }
    }
}

fn trans(cfg: &cfg::CFG) {
    let module = unsafe { ffi::BinaryenModuleCreate() };
    let param_types: Vec<ffi::BinaryenType> = vec![];
    let procedure_fn_ty = unsafe {
        ffi::BinaryenAddFunctionType(
            module, 
            ptr::null_mut(),
            ffi::BinaryenNone(),
            param_types.as_ptr() as _,
            param_types.len() as _
        )
    };
    let mut ctx = TransCtx {
        module,
        cfg,
        c_strings: Vec::new(),
        procedure_fn_ty
    };

    

    let mut binaryen_routines = HashMap::new();
    for routine_id in ctx.cfg.subroutines().keys() {
        println!("translating {:?}", routine_id);
        let mut start_routine_ctx = RoutineTransCtx::new(&mut ctx, *routine_id);
        let binaryen_fn_ref = start_routine_ctx.trans();

        binaryen_routines.insert(routine_id, binaryen_fn_ref);
    }

    unsafe {
        let start_binaryen_fn = binaryen_routines[&ctx.cfg.start()];
        ffi::BinaryenSetStart(module, start_binaryen_fn);
    
        if ffi::BinaryenModuleValidate(module) == 0 {
            panic!("module is not valid");
        }

        let mut buf = Vec::<u8>::with_capacity(8192);
        let written = ffi::BinaryenModuleWrite(module, buf.as_ptr() as _, 8192);
        if written == buf.len() {
            panic!("overflow?");
        }

        use std::io::Write;

        let mut file = File::create("foo.txt").unwrap();
        file.write_all(&buf).unwrap();
    }
}

struct RoutineTransCtx<'t> {
    module: ffi::BinaryenModuleRef,
    routine_id: cfg::RoutineId,
    routine: &'t cfg::Routine,
    relooper: ffi::RelooperRef,
    c_strings: &'t mut Vec<CString>,
    procedure_fn_ty: ffi::BinaryenFunctionTypeRef,
}

impl<'t> RoutineTransCtx<'t> {
    fn new(ctx: &'t mut TransCtx, routine_id: cfg::RoutineId) -> RoutineTransCtx<'t> {
        let routine = &ctx.cfg.subroutines()[&routine_id];
        let relooper = unsafe { ffi::RelooperCreate() };

        RoutineTransCtx {
            module: ctx.module,
            routine,
            routine_id,
            relooper,
            c_strings: &mut ctx.c_strings,
            procedure_fn_ty: ctx.procedure_fn_ty,
        }
    }

    fn trans(&mut self) -> ffi::BinaryenFunctionRef {
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
                Skip { predicate, next, skip } => unsafe {
                    let predicate_expr = self.trans_predicate(predicate);
                    let skip_relooper_block = relooper_blocks[&skip];
                    let next_relooper_block = relooper_blocks[&next];

                    ffi::RelooperAddBranch(
                        from_relooper_block,
                        skip_relooper_block,
                        predicate_expr,
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

            let routine_name = CString::new(func_name_from_addr(self.routine_id.0)).unwrap();
            let routine_name_ptr = routine_name.as_ptr();
            self.c_strings.push(routine_name);

            let var_types: Vec<ffi::BinaryenType> = vec![];

            ffi::BinaryenAddFunction(
                self.module,
                routine_name_ptr,
                self.procedure_fn_ty,
                var_types.as_ptr() as _,
                var_types.len() as _,
                body_code
            )
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
            // ffi::BinaryenExpressionPrint(code);
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
            Instruction::Call(addr) => {
                let routine_name = CString::new(func_name_from_addr(addr)).unwrap();
                let routine_name_ptr = routine_name.as_ptr();
                self.c_strings.push(routine_name);

                let call_expr = ffi::BinaryenCall(
                    self.module,
                    routine_name_ptr,
                    ptr::null_mut(),
                    0 as _,
                    ffi::BinaryenNone()
                );
                stmts.push(call_expr);
            }
            Instruction::ClearScreen => {
                let clear_expr =
                    self.trans_call_import("clear_screen", vec![], ffi::BinaryenNone());
                stmts.push(clear_expr);
            }
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
            Instruction::Apply { vx, vy, f } => {
                self.trans_apply(vx, vy, f, stmts);
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
                let add_expr =
                    ffi::BinaryenBinary(self.module, ffi::BinaryenAddInt32(), vx_expr, load_i_expr);
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
                let set_st_expr =
                    self.trans_call_import("set_st", vec![load_expr], ffi::BinaryenNone());
                // TODO: Drop?
                stmts.push(set_st_expr);
            }
            Instruction::SetDT(vx) => {
                let load_expr = self.load_reg(vx);
                let set_dt_expr =
                    self.trans_call_import("set_dt", vec![load_expr], ffi::BinaryenNone());
                // TODO: Drop?
                stmts.push(set_dt_expr);
            }
            Instruction::WaitKey(vx) => {
                let wait_key_expr =
                    self.trans_call_import("wait_key", vec![], ffi::BinaryenInt32());
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
                    let increment_i_expr = ffi::BinaryenBinary(
                        self.module,
                        ffi::BinaryenAddInt32(),
                        imm_1_expr,
                        load_i_expr,
                    );
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
                    let increment_i_expr = ffi::BinaryenBinary(
                        self.module,
                        ffi::BinaryenAddInt32(),
                        imm_1_expr,
                        load_i_expr,
                    );
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

    unsafe fn trans_apply(
        &mut self,
        vx: Reg,
        vy: Reg,
        f: Fun,
        stmts: &mut Vec<ffi::BinaryenExpressionRef>,
    ) {
        let vx_expr = self.load_reg(vx);
        let vy_expr = self.load_reg(vy);

        match f {
            Fun::Id => {
                stmts.push(self.store_reg(vx, vy_expr));
            }
            Fun::Or => {
                let or_expr =
                    ffi::BinaryenBinary(self.module, ffi::BinaryenOrInt32(), vx_expr, vy_expr);
                stmts.push(or_expr);
            }
            Fun::And => {
                let and_expr =
                    ffi::BinaryenBinary(self.module, ffi::BinaryenAndInt32(), vx_expr, vy_expr);
                stmts.push(and_expr);
            }
            Fun::Xor => {
                let xor_expr =
                    ffi::BinaryenBinary(self.module, ffi::BinaryenXorInt32(), vx_expr, vy_expr);
                stmts.push(xor_expr);
            }
            Fun::Add => {
                // u32 $tmp = x + y
                // $result = (tmp & 0xFFFF)
                // $overflow = (tmp != result)

                let add_expr =
                    ffi::BinaryenBinary(self.module, ffi::BinaryenAddInt32(), vx_expr, vy_expr);
                let tee_tmp_expr = ffi::BinaryenTeeLocal(self.module, TMP_LOCAL, add_expr);
                let mask_imm_expr = self.load_imm(0xFFFF);
                let mask_expr = ffi::BinaryenBinary(
                    self.module,
                    ffi::BinaryenAndInt32(),
                    tee_tmp_expr,
                    mask_imm_expr,
                );
                let store_result_expr = self.store_reg(vx, mask_expr);

                stmts.push(store_result_expr);

                let load_tmp_expr =
                    ffi::BinaryenGetLocal(self.module, TMP_LOCAL, ffi::BinaryenInt32());
                let load_result_expr = self.load_reg(vx);
                let overflow_expr = ffi::BinaryenBinary(
                    self.module,
                    ffi::BinaryenNeInt32(),
                    load_tmp_expr,
                    load_result_expr,
                );
                let store_overflow_expr = self.store_reg(Reg::Vf, overflow_expr);

                stmts.push(store_overflow_expr);
            }
            Fun::Subtract => {
                let add_expr =
                    ffi::BinaryenBinary(self.module, ffi::BinaryenSubInt32(), vx_expr, vy_expr);
                let tee_tmp_expr = ffi::BinaryenTeeLocal(self.module, TMP_LOCAL, add_expr);
                let mask_imm_expr = self.load_imm(0xFFFF);
                let mask_expr = ffi::BinaryenBinary(
                    self.module,
                    ffi::BinaryenAndInt32(),
                    tee_tmp_expr,
                    mask_imm_expr,
                );
                let store_result_expr = self.store_reg(vx, mask_expr);

                stmts.push(store_result_expr);

                let load_tmp_expr =
                    ffi::BinaryenGetLocal(self.module, TMP_LOCAL, ffi::BinaryenInt32());
                let load_result_expr = self.load_reg(vx);
                let overflow_expr = ffi::BinaryenBinary(
                    self.module,
                    ffi::BinaryenNeInt32(),
                    load_tmp_expr,
                    load_result_expr,
                );
                let store_overflow_expr = self.store_reg(Reg::Vf, overflow_expr);

                stmts.push(store_overflow_expr);
            }
            Fun::ShiftRight => {
                // $result = y >> 1
                // $vf = y & 0x01
                let imm_1_expr = self.load_imm(1);
                let shift_expr =
                    ffi::BinaryenBinary(self.module, ffi::BinaryenShrUInt32(), vy_expr, imm_1_expr);
                let store_result_expr = self.store_reg(vx, shift_expr);
                stmts.push(store_result_expr);

                // TODO: It is OK to use same expr twice???
                let mask_expr =
                    ffi::BinaryenBinary(self.module, ffi::BinaryenAndInt32(), vy_expr, imm_1_expr);
                let store_vf_expr = self.store_reg(Reg::Vf, mask_expr);
                stmts.push(store_vf_expr);
            }
            Fun::SubtractInv => {
                let add_expr =
                    ffi::BinaryenBinary(self.module, ffi::BinaryenSubInt32(), vy_expr, vx_expr);
                let tee_tmp_expr = ffi::BinaryenTeeLocal(self.module, TMP_LOCAL, add_expr);
                let mask_imm_expr = self.load_imm(0xFFFF);
                let mask_expr = ffi::BinaryenBinary(
                    self.module,
                    ffi::BinaryenAndInt32(),
                    tee_tmp_expr,
                    mask_imm_expr,
                );
                let store_result_expr = self.store_reg(vx, mask_expr);

                stmts.push(store_result_expr);

                let load_tmp_expr =
                    ffi::BinaryenGetLocal(self.module, TMP_LOCAL, ffi::BinaryenInt32());
                let load_result_expr = self.load_reg(vx);
                let overflow_expr = ffi::BinaryenBinary(
                    self.module,
                    ffi::BinaryenNeInt32(),
                    load_tmp_expr,
                    load_result_expr,
                );
                let store_overflow_expr = self.store_reg(Reg::Vf, overflow_expr);

                stmts.push(store_overflow_expr);
            }
            Fun::ShiftLeft => {
                // $result = y << 1;
                // $vf = y << 1;
                let imm_1_expr = self.load_imm(1);
                let shift_expr =
                    ffi::BinaryenBinary(self.module, ffi::BinaryenShlInt32(), vy_expr, imm_1_expr);
                let store_result_expr = self.store_reg(vx, shift_expr);
                stmts.push(store_result_expr);

                // TODO: It is OK to use same expr twice???
                let mask_expr =
                    ffi::BinaryenBinary(self.module, ffi::BinaryenShlInt32(), vy_expr, imm_1_expr);
                let store_vf_expr = self.store_reg(Reg::Vf, mask_expr);
                stmts.push(store_vf_expr);
            }
            _ => panic!(),
        }
    }

    fn trans_call_import(
        &mut self,
        name: &str,
        operands: Vec<ffi::BinaryenExpressionRef>,
        result_ty: ffi::BinaryenType,
    ) -> ffi::BinaryenExpressionRef {
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

    fn trans_predicate(&mut self, predicate: Predicate) -> ffi::BinaryenExpressionRef {
        let (lhs, rhs) = match predicate.cond {
            Condition::Reg(vx, vy) => {
                let vx_expr = self.load_reg(vx);
                let vy_expr = self.load_reg(vy);
                (vx_expr, vy_expr)
            },
            Condition::Imm(vx, imm) => {
                let vx_expr = self.load_reg(vx);
                let imm_expr = self.load_imm(imm.0 as _);
                (vx_expr, imm_expr)
            },
            Condition::Pressed(vx) => {
                let vx_expr = self.load_reg(vx);
                let pressed_expr = unsafe {
                    self.trans_call_import("is_key_pressed", vec![vx_expr], ffi::BinaryenInt32())
                };
                (pressed_expr, self.load_imm(1))
            },
        };
        unsafe { 
            let cmp_op = match predicate.cmp {
                Cmp::Eq => ffi::BinaryenEqInt32(),
                Cmp::Ne => ffi::BinaryenNeInt32(),
            };
            ffi::BinaryenBinary(self.module, cmp_op, lhs, rhs) 
        }
    }

    fn load_i(&mut self) -> ffi::BinaryenExpressionRef {
        unsafe {
            ffi::BinaryenLoad(
                self.module,
                2,
                0,
                0,
                0,
                ffi::BinaryenInt32(),
                self.load_imm(I_MEM_ADDR),
            )
        }
    }

    fn store_i(&mut self, value: ffi::BinaryenExpressionRef) -> ffi::BinaryenExpressionRef {
        unsafe {
            ffi::BinaryenStore(
                self.module,
                2,
                0,
                0,
                self.load_imm(I_MEM_ADDR),
                value,
                ffi::BinaryenInt32(),
            )
        }
    }

    fn load_reg(&mut self, reg: Reg) -> ffi::BinaryenExpressionRef {
        unsafe {
            let reg_ptr: u32 = reg_mem_addr(reg);
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
            let reg_ptr: u32 = reg_mem_addr(reg);
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
        unsafe { ffi::BinaryenLoad(self.module, 1, 0, 0, 0, ffi::BinaryenInt32(), i_expr) }
    }

    fn store_mem_at_i(&mut self, value: ffi::BinaryenExpressionRef) -> ffi::BinaryenExpressionRef {
        let i_expr = self.load_i();
        unsafe { ffi::BinaryenStore(self.module, 1, 0, 0, i_expr, value, ffi::BinaryenInt32()) }
    }
}


fn func_name_from_addr(addr: Addr) -> String {
    format!("routine_{}", addr.0)
}

#[cfg_attr(rustfmt, rustfmt_skip)]
const FONT_SPRITES: [u8; 80] = [
	0xF0, 0x90, 0x90, 0x90, 0xF0, // 0
	0x20, 0x60, 0x20, 0x20, 0x70, // 1
	0xF0, 0x10, 0xF0, 0x80, 0xF0, // 2
	0xF0, 0x10, 0xF0, 0x10, 0xF0, // 3
	0x90, 0x90, 0xF0, 0x10, 0x10, // 4
	0xF0, 0x80, 0xF0, 0x10, 0xF0, // 5
	0xF0, 0x80, 0xF0, 0x90, 0xF0, // 6
	0xF0, 0x10, 0x20, 0x40, 0x40, // 7
	0xF0, 0x90, 0xF0, 0x90, 0xF0, // 8
	0xF0, 0x90, 0xF0, 0x10, 0xF0, // 9
	0xF0, 0x90, 0xF0, 0x90, 0x90, // A
	0xE0, 0x90, 0xE0, 0x90, 0xE0, // B
	0xF0, 0x80, 0x80, 0x80, 0xF0, // C
	0xE0, 0x90, 0x90, 0x90, 0xE0, // D
	0xF0, 0x80, 0xF0, 0x80, 0xF0, // E
	0xF0, 0x80, 0xF0, 0x80, 0x80, // F
];

// 2 bytes
const I_MEM_ADDR: u32 = 80;

fn reg_mem_addr(reg: Reg) -> u32 {
    (reg.index() as u32) + 80 + 2
}

const TMP_LOCAL: ffi::BinaryenIndex = 0;
