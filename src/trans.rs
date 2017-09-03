
use instruction::*;
use binaryen::*;
use cfg;

use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Opts<'a> {
    pub out_file: Option<&'a str>,
    pub optimize: bool,
    pub print: bool,
}

impl<'a> Default for Opts<'a> {
    fn default() -> Opts<'a> {
        Opts {
            out_file: None,
            optimize: true,
            print: false
        }
    }
}

pub fn trans_rom(rom: &[u8], cfg: &cfg::CFG, opts: Opts) {
    let mut builder = Module::new();

    let procedure_fn_ty = builder.add_fn_type(None, &[], Ty::none());

    let mut ctx = TransCtx {
        builder: &mut builder,
        cfg,
        procedure_fn_ty,
    };

    {
        let font_segment = Segment::new(&FONT_SPRITES, ctx.builder.const_(Literal::I32(0)));

        // We need to put ROM data at 0x200 (entry address) because some roms
        // might actually read this data.
        let rom_segment = Segment::new(rom, ctx.builder.const_(Literal::I32(0x200)));

        let segments = &[font_segment, rom_segment];
        ctx.builder.set_memory(1, 1, Some(&"mem".into()), segments);
    }

    ctx.add_import("clear_screen", &[], Ty::none());
    ctx.add_import("random", &[], Ty::value(ValueTy::I32));
    ctx.add_import(
        "draw",
        &[ValueTy::I32, ValueTy::I32, ValueTy::I32, ValueTy::I32],
        Ty::value(ValueTy::I32),
    );
    ctx.add_import("get_dt", &[], Ty::value(ValueTy::I32));
    ctx.add_import("set_dt", &[ValueTy::I32], Ty::none());
    ctx.add_import("set_st", &[ValueTy::I32], Ty::none());
    ctx.add_import("wait_key", &[], Ty::value(ValueTy::I32));
    ctx.add_import("store_bcd", &[ValueTy::I32, ValueTy::I32], Ty::none());
    ctx.add_import("is_key_pressed", &[ValueTy::I32], Ty::value(ValueTy::I32));

    let reg_i_init = ctx.builder.const_(Literal::I32(0));
    ctx.builder
        .add_global(&"regI".into(), ValueTy::I32, true, reg_i_init);

    for i in 0..16 {
        let reg = Reg::from_index(i);
        let reg_name = get_reg_name(reg);
        let init_expr = ctx.builder.const_(Literal::I32(0));
        ctx.builder
            .add_global(&reg_name.into(), ValueTy::I32, true, init_expr);
    }

    let mut binaryen_routines = HashMap::new();
    let subroutines = ctx.cfg.subroutines().keys();
    println!("subs {:#?}", subroutines);
    for routine_id in subroutines {
        println!("translating {:?}", routine_id);
        let start_routine_ctx = RoutineTransCtx::new(&mut ctx, *routine_id);
        let binaryen_fn_ref = start_routine_ctx.trans();

        binaryen_routines.insert(routine_id, binaryen_fn_ref);
    }

    // let start_binaryen_fn = &binaryen_routines[&ctx.cfg.start()];
    // ctx.builder.set_start(start_binaryen_fn);

    ctx.builder
        .add_export(&"routine_512".into(), &"routine_512".into());

    if !ctx.builder.is_valid() {
        panic!("module is not valid");
    }

    if opts.optimize {
        ctx.builder.optimize();
    }

    if opts.print {
        ctx.builder.print();
    }

    if let Some(out_file) = opts.out_file {
        let buf = ctx.builder.write();
        dump(&buf, out_file);
    }
}

fn dump(buf: &[u8], filename: &str) {
    use std::fs::File;
    use std::io::Write;
    let mut file = File::create(filename).unwrap();
    file.write_all(buf).unwrap();
}

struct TransCtx<'a> {
    builder: &'a mut Module,
    cfg: &'a cfg::CFG,
    procedure_fn_ty: FnType,
}

impl<'a> TransCtx<'a> {
    fn add_import(&mut self, name: &str, param_tys: &[ValueTy], result_ty: Ty) {
        let fn_ty = self.builder.add_fn_type(None, param_tys, result_ty);
        self.builder
            .add_import(&name.into(), &"env".into(), &name.into(), &fn_ty)
    }
}

struct RoutineTransCtx<'t> {
    builder: &'t mut Module,
    routine_id: cfg::RoutineId,
    routine: &'t cfg::Routine,
    procedure_fn_ty: &'t FnType,
}

impl<'t> RoutineTransCtx<'t> {
    fn new(ctx: &'t mut TransCtx, routine_id: cfg::RoutineId) -> RoutineTransCtx<'t> {
        let routine = &ctx.cfg.subroutines()[&routine_id];

        RoutineTransCtx {
            builder: &mut ctx.builder,
            routine,
            routine_id,
            procedure_fn_ty: &ctx.procedure_fn_ty,
        }
    }

    fn trans(mut self) -> FnRef {
        let mut relooper = Relooper::new();
        let mut relooper_blocks: HashMap<cfg::BasicBlockId, RelooperBlockId> = HashMap::new();
        for bb_id in self.routine.bbs.keys() {
            let code = self.trans_bb(*bb_id);
            let relooper_block = relooper.add_block(code);
            relooper_blocks.insert(*bb_id, relooper_block);
        }

        for bb_id in self.routine.bbs.keys() {
            let bb = &self.routine.bbs[bb_id];
            let from_relooper_block = relooper_blocks[bb_id];

            use cfg::Terminator::*;
            match bb.terminator() {
                Ret => {
                    // Return should be added in trans_bb()
                }
                Jump { target } => {
                    let to_relooper_block = relooper_blocks[&target];
                    relooper.add_branch(from_relooper_block, to_relooper_block, None, None);
                }
                Skip {
                    predicate,
                    next,
                    skip,
                } => {
                    let predicate_expr = self.trans_predicate(predicate);
                    let skip_relooper_block = relooper_blocks[&skip];
                    let next_relooper_block = relooper_blocks[&next];

                    relooper.add_branch(
                        from_relooper_block,
                        skip_relooper_block,
                        Some(predicate_expr),
                        None,
                    );
                    relooper.add_branch(from_relooper_block, next_relooper_block, None, None);
                }
            }
        }

        let relooper_entry_block = relooper_blocks[&self.routine.entry];

        let body_code = relooper.render(self.builder, relooper_entry_block, LABEL_HELPER_LOCAL);
        let var_types = &[ValueTy::I32, ValueTy::I32];
        self.builder.add_fn(
            &func_name_from_addr(self.routine_id.0).into(),
            self.procedure_fn_ty,
            var_types,
            body_code,
        )
    }

    fn trans_bb(&mut self, bb_id: cfg::BasicBlockId) -> Expr {
        let bb = &self.routine.bbs[&bb_id];

        println!("{:#?}", bb);

        let mut stmts = Vec::new();

        for inst in bb.instructions() {
            self.trans_instruction(inst, &mut stmts);
        }

        // Handle Ret terminator here to make Relooper's life easier...
        if let cfg::Terminator::Ret = bb.terminator() {
            stmts.push(self.builder.ret(None));
        }

        self.builder.block(None, &stmts, Ty::none())
    }

    fn trans_instruction(&mut self, instruction: &Instruction, stmts: &mut Vec<Expr>) {
        match *instruction {
            Instruction::Call(addr) => {
                let routine_name = func_name_from_addr(addr).into();
                let call_expr = self.builder.call(&routine_name, &[]);
                stmts.push(call_expr);
            }
            Instruction::ClearScreen => {
                let clear_expr = self.trans_call_import("clear_screen", &[], Ty::none());
                stmts.push(clear_expr);
            }
            Instruction::PutImm { vx, imm } => {
                let imm_expr = self.load_imm(imm.0 as u32);
                stmts.push(self.store_reg(vx, imm_expr));
            }
            Instruction::AddImm { vx, imm } => {
                let imm_expr = self.load_imm(imm.0 as u32);
                let load_expr = self.load_reg(vx);
                let add_expr = self.builder.binary(BinaryOp::AddI32, load_expr, imm_expr);
                let mask_imm_expr = self.load_imm(0xFF);
                let mask_expr = self.builder
                    .binary(BinaryOp::AndI32, add_expr, mask_imm_expr);
                let store_result_expr = self.store_reg(vx, mask_expr);
                stmts.push(store_result_expr);
            }
            Instruction::Apply { vx, vy, f } => {
                self.trans_apply(vx, vy, f, stmts);
            }
            Instruction::Randomize { vx, imm } => {
                let rnd_expr = self.trans_call_import("random", &[], Ty::value(ValueTy::I32));
                let mask_imm_expr = self.load_imm(imm.0 as u32);
                let mask_expr = self.builder
                    .binary(BinaryOp::AndI32, rnd_expr, mask_imm_expr);
                let store_expr = self.store_reg(vx, mask_expr);
                stmts.push(store_expr);
            }
            Instruction::Draw { vx, vy, n } => {
                let x_expr = self.load_reg(vx);
                let y_expr = self.load_reg(vy);
                let load_i_expr = self.load_i();
                let n_expr = self.load_imm(n.0 as u32);

                let operands = &[x_expr, y_expr, load_i_expr, n_expr];
                let draw_expr = self.trans_call_import("draw", operands, Ty::value(ValueTy::I32));
                let store_expr = self.store_reg(Reg::Vf, draw_expr);

                stmts.push(store_expr);
            }
            Instruction::SetI(addr) => {
                let imm_expr = self.load_imm(addr.0 as u32 & 0xFFF);
                let store_i_expr = self.store_i(imm_expr);
                stmts.push(store_i_expr);
            }
            Instruction::AddI(vx) => {
                let vx_expr = self.load_reg(vx);
                let load_i_expr = self.load_i();
                let add_expr = self.builder.binary(BinaryOp::AddI32, vx_expr, load_i_expr);
                let mask_imm_expr = self.load_imm(0xFFFF);
                let mask_expr = self.builder
                    .binary(BinaryOp::AndI32, add_expr, mask_imm_expr);
                let store_i_expr = self.store_i(mask_expr);
                stmts.push(store_i_expr);
            }
            Instruction::GetDT(vx) => {
                let get_dt_expr = self.trans_call_import("get_dt", &[], Ty::value(ValueTy::I32));
                let store_expr = self.store_reg(vx, get_dt_expr);
                stmts.push(store_expr);
            }
            Instruction::SetST(vx) => {
                let load_expr = self.load_reg(vx);
                let set_st_expr = self.trans_call_import("set_st", &[load_expr], Ty::none());
                stmts.push(set_st_expr);
            }
            Instruction::SetDT(vx) => {
                let load_expr = self.load_reg(vx);
                let set_dt_expr = self.trans_call_import("set_dt", &[load_expr], Ty::none());
                stmts.push(set_dt_expr);
            }
            Instruction::WaitKey(vx) => {
                let wait_key_expr =
                    self.trans_call_import("wait_key", &[], Ty::value(ValueTy::I32));
                let store_expr = self.store_reg(vx, wait_key_expr);
                stmts.push(store_expr);
            }
            Instruction::LoadRegs(vx) => for offset in 0..(vx.index() as usize + 1) {
                let reg = Reg::from_index(offset as u8);
                let load_mem_expr = self.load_mem_at_i(offset as u32);
                let store_reg_expr = self.store_reg(reg, load_mem_expr);
                stmts.push(store_reg_expr);
            },
            Instruction::StoreRegs(vx) => for offset in 0..(vx.index() as usize + 1) {
                let reg = Reg::from_index(offset as u8);
                let load_reg_expr = self.load_reg(reg);
                let store_mem_expr = self.store_mem_at_i(load_reg_expr, offset as u32);
                stmts.push(store_mem_expr);
            },
            Instruction::LoadGlyph(vx) => {
                // (vx * 5)
                let vx_expr = self.load_reg(vx);
                let imm_expr = self.load_imm(0x05);
                let result_expr = self.builder
                    .binary(BinaryOp::MulI32, vx_expr, imm_expr);
                let store_i_expr = self.store_i(result_expr);
                stmts.push(store_i_expr);
            }
            Instruction::StoreBCD(vx) => {
                let vx_expr = self.load_reg(vx);
                let i_expr = self.load_i();
                let bcd_expr = self.trans_call_import("store_bcd", &[vx_expr, i_expr], Ty::none());
                stmts.push(bcd_expr);
            }
            _ => panic!("unimplemented: {:#?}", instruction),
        }
    }

    fn trans_apply(&mut self, vx: Reg, vy: Reg, f: Fun, stmts: &mut Vec<Expr>) {
        let vx_expr = self.load_reg(vx);
        let vy_expr = self.load_reg(vy);

        match f {
            Fun::Id => {
                stmts.push(self.store_reg(vx, vy_expr));
            }
            Fun::Or => {
                let or_expr = self.builder.binary(BinaryOp::OrI32, vx_expr, vy_expr);
                let store_expr = self.store_reg(vx, or_expr);
                stmts.push(store_expr);
            }
            Fun::And => {
                let and_expr = self.builder.binary(BinaryOp::AndI32, vx_expr, vy_expr);
                let store_expr = self.store_reg(vx, and_expr);
                stmts.push(store_expr);
            }
            Fun::Xor => {
                let xor_expr = self.builder.binary(BinaryOp::XorI32, vx_expr, vy_expr);
                let store_expr = self.store_reg(vx, xor_expr);
                stmts.push(store_expr);
            }
            Fun::Add => {
                // u32 $tmp = x + y
                // $result = (tmp & 0xFFFF)
                // $overflow = (tmp != result)

                let add_expr = self.builder.binary(BinaryOp::AddI32, vx_expr, vy_expr);
                let tee_tmp_expr = self.builder.tee_local(TMP_LOCAL, add_expr);
                let mask_imm_expr = self.load_imm(0xFF);
                let mask_expr = self.builder
                    .binary(BinaryOp::AndI32, tee_tmp_expr, mask_imm_expr);
                let store_result_expr = self.store_reg(vx, mask_expr);

                stmts.push(store_result_expr);

                let load_tmp_expr = self.builder.get_local(TMP_LOCAL, ValueTy::I32);
                let load_result_expr = self.load_reg(vx);
                let overflow_expr =
                    self.builder
                        .binary(BinaryOp::NeI32, load_tmp_expr, load_result_expr);
                let store_overflow_expr = self.store_reg(Reg::Vf, overflow_expr);

                stmts.push(store_overflow_expr);
            }
            Fun::Subtract => {
                let add_expr = self.builder.binary(BinaryOp::SubI32, vx_expr, vy_expr);
                let tee_tmp_expr = self.builder.tee_local(TMP_LOCAL, add_expr);
                let mask_imm_expr = self.load_imm(0xFF);
                let mask_expr = self.builder
                    .binary(BinaryOp::AndI32, tee_tmp_expr, mask_imm_expr);
                let store_result_expr = self.store_reg(vx, mask_expr);

                stmts.push(store_result_expr);

                let load_tmp_expr = self.builder.get_local(TMP_LOCAL, ValueTy::I32);
                let load_result_expr = self.load_reg(vx);
                let overflow_expr =
                    self.builder
                        .binary(BinaryOp::NeI32, load_tmp_expr, load_result_expr);
                let store_overflow_expr = self.store_reg(Reg::Vf, overflow_expr);

                stmts.push(store_overflow_expr);
            }
            Fun::ShiftRight => {
                // $result = y >> 1
                // $vf = y & 0x01
                let imm_1_expr = self.load_imm(1);
                let shift_expr = self.builder.binary(BinaryOp::ShrUI32, vy_expr, imm_1_expr);
                let store_result_expr = self.store_reg(vx, shift_expr);
                stmts.push(store_result_expr);

                let vy_expr = self.load_reg(vy);
                let imm_1_expr = self.load_imm(1);
                let mask_expr = self.builder.binary(BinaryOp::AndI32, vy_expr, imm_1_expr);
                let store_vf_expr = self.store_reg(Reg::Vf, mask_expr);
                stmts.push(store_vf_expr);
            }
            Fun::SubtractInv => {
                let add_expr = self.builder.binary(BinaryOp::SubI32, vy_expr, vx_expr);
                let tee_tmp_expr = self.builder.tee_local(TMP_LOCAL, add_expr);
                let mask_imm_expr = self.load_imm(0xFF);
                let mask_expr = self.builder
                    .binary(BinaryOp::AndI32, tee_tmp_expr, mask_imm_expr);
                let store_result_expr = self.store_reg(vx, mask_expr);

                stmts.push(store_result_expr);

                let load_tmp_expr = self.builder.get_local(TMP_LOCAL, ValueTy::I32);
                let load_result_expr = self.load_reg(vx);
                let overflow_expr =
                    self.builder
                        .binary(BinaryOp::NeI32, load_tmp_expr, load_result_expr);
                let store_overflow_expr = self.store_reg(Reg::Vf, overflow_expr);

                stmts.push(store_overflow_expr);
            }
            Fun::ShiftLeft => {
                // $result = y << 1;
                // $vf = y << 1;
                let imm_1_expr = self.load_imm(1);
                let shift_expr = self.builder.binary(BinaryOp::ShlI32, vy_expr, imm_1_expr);
                let store_result_expr = self.store_reg(vx, shift_expr);
                stmts.push(store_result_expr);

                let imm_1_expr = self.load_imm(1);
                let vy_expr = self.load_reg(vy);
                let mask_expr = self.builder.binary(BinaryOp::ShlI32, vy_expr, imm_1_expr);
                let store_vf_expr = self.store_reg(Reg::Vf, mask_expr);
                stmts.push(store_vf_expr);
            }
        }
    }

    fn trans_call_import(&mut self, name: &str, operands: &[Expr], result_ty: Ty) -> Expr {
        self.builder.call_import(&name.into(), operands, result_ty)
    }

    fn trans_predicate(&mut self, predicate: Predicate) -> Expr {
        let (lhs, rhs) = match predicate.cond {
            Condition::Reg(vx, vy) => {
                let vx_expr = self.load_reg(vx);
                let vy_expr = self.load_reg(vy);
                (vx_expr, vy_expr)
            }
            Condition::Imm(vx, imm) => {
                let vx_expr = self.load_reg(vx);
                let imm_expr = self.load_imm(imm.0 as _);
                (vx_expr, imm_expr)
            }
            Condition::Pressed(vx) => {
                let vx_expr = self.load_reg(vx);
                let pressed_expr =
                    self.trans_call_import("is_key_pressed", &[vx_expr], Ty::value(ValueTy::I32));
                (pressed_expr, self.load_imm(1))
            }
        };
        let cmp_op = match predicate.cmp {
            Cmp::Eq => BinaryOp::EqI32,
            Cmp::Ne => BinaryOp::NeI32,
        };
        self.builder.binary(cmp_op, lhs, rhs)
    }

    fn load_i(&mut self) -> Expr {
        self.builder.get_global(&"regI".into(), ValueTy::I32)
    }

    fn store_i(&mut self, value: Expr) -> Expr {
        self.builder.set_global(&"regI".into(), value)
    }

    fn load_reg(&mut self, reg: Reg) -> Expr {
        let reg_name = get_reg_name(reg);
        self.builder.get_global(&reg_name.into(), ValueTy::I32)
    }

    fn store_reg(&mut self, reg: Reg, value: Expr) -> Expr {
        let reg_name = get_reg_name(reg);
        self.builder.set_global(&reg_name.into(), value)
    }

    fn load_imm(&mut self, c: u32) -> Expr {
        self.builder.const_(Literal::I32(c))
    }

    fn load_mem_at_i(&mut self, offset: u32) -> Expr {
        let i_expr = self.load_i();
        self.builder.load(1, false, offset, 0, ValueTy::I32, i_expr)
    }

    fn store_mem_at_i(&mut self, value: Expr, offset: u32) -> Expr {
        let i_expr = self.load_i();
        self.builder.store(1, offset, 0, i_expr, value, ValueTy::I32)
    }
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

const TMP_LOCAL: u32 = 0;
const LABEL_HELPER_LOCAL: u32 = 1;

fn func_name_from_addr(addr: Addr) -> String {
    format!("routine_{}", addr.0)
}

fn get_reg_name(reg: Reg) -> String {
    format!("V{}", reg.index())
}
