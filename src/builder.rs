use binaryen::ffi;
use std::ffi::CString;
use std::cell::RefCell;
use std::rc::Rc;
use std::os::raw::{c_char, c_int};
use std::ptr;

struct InnerModule {
    module: ffi::BinaryenModuleRef,
    c_strings: RefCell<Vec<CString>>,
}

impl Drop for InnerModule {
    fn drop(&mut self) {
        unsafe { ffi::BinaryenModuleDispose(self.module) }
    }
}

pub struct Module {
    inner: Rc<InnerModule>,
}

impl Module {
    fn save_string_and_return_ptr(&self, string: CString) -> *const c_char {
        let str_ptr = string.as_ptr();
        self.inner.c_strings.borrow_mut().push(string);
        str_ptr
    }

    pub fn from_raw(module: ffi::BinaryenModuleRef) -> Module {
        Module {
            inner: Rc::new(InnerModule {
                module,
                c_strings: RefCell::new(Vec::new()),
            }),
        }
    }

    pub fn auto_drop(&mut self) {
        unsafe {
            ffi::BinaryenModuleAutoDrop(self.inner.module);
        }
    }

    pub fn optimize(&mut self) {
        unsafe { ffi::BinaryenModuleOptimize(self.inner.module) }
    }

    pub fn is_valid(&mut self) -> bool {
        unsafe { ffi::BinaryenModuleValidate(self.inner.module) == 1 }
    }

    pub fn print(&self) {
        unsafe { ffi::BinaryenModulePrint(self.inner.module) }
    }

    pub fn set_start(&mut self, fn_ref: &FnRef) {
        unsafe {
            ffi::BinaryenSetStart(self.inner.module, fn_ref.inner);
        }
    }

    pub fn new_fn_type(&self, name: Option<CString>, ty: Ty, param_tys: Vec<ValueTy>) -> FnType {
        let inner = unsafe {
            let name_ptr = name.map_or(ptr::null(), |n| self.save_string_and_return_ptr(n));
            let mut param_tys_raw = param_tys
                .into_iter()
                .map(|ty| ty.into())
                .collect::<Vec<_>>();
            ffi::BinaryenAddFunctionType(
                self.inner.module,
                name_ptr,
                ty.into(),
                param_tys_raw.as_mut_ptr(),
                param_tys_raw.len() as _,
            )
        };
        FnType { inner }
    }

    pub fn new_fn(
        &self,
        name: CString,
        fn_ty: &FnType,
        var_tys: Vec<ValueTy>,
        body: Expr,
    ) -> FnRef {
        let inner = unsafe {
            let name_ptr = self.save_string_and_return_ptr(name);
            let mut var_tys_raw = var_tys.into_iter().map(|ty| ty.into()).collect::<Vec<_>>();
            ffi::BinaryenAddFunction(
                self.inner.module,
                name_ptr,
                fn_ty.inner,
                var_tys_raw.as_mut_ptr(),
                var_tys_raw.len() as _,
                body.into_raw(),
            )
        };
        FnRef { inner }
    }

    pub fn new_global(&self, name: CString, ty: ValueTy, mutable: bool, init: Expr) {
        let name_ptr = self.save_string_and_return_ptr(name);
        unsafe {
            ffi::BinaryenAddGlobal(
                self.inner.module,
                name_ptr,
                ty.into(),
                mutable as c_int,
                init.into_raw(),
            );
        }
    }

    // TODO: undefined ty?
    // https://github.com/WebAssembly/binaryen/blob/master/src/binaryen-c.h#L272
    pub fn block(&mut self, name: Option<CString>, mut children: Vec<Expr>, ty: Ty) -> Expr {
        let name_ptr = name.map_or(ptr::null(), |n| self.save_string_and_return_ptr(n));

        let raw_expr = unsafe {
            ffi::BinaryenBlock(
                self.inner.module,
                name_ptr,
                children.as_mut_ptr() as _,
                children.len() as _,
                ty.into(),
            )
        };
        Expr::from_raw(self, raw_expr)
    }

    pub fn const_literal(&mut self, literal: Literal) -> Expr {
        let raw_expr = unsafe { ffi::BinaryenConst(self.inner.module, literal.into()) };
        Expr::from_raw(self, raw_expr)
    }

    pub fn load(
        &mut self,
        bytes: u32,
        signed: bool,
        offset: u32,
        align: u32,
        ty: ValueTy,
        ptr: Expr,
    ) -> Expr {
        let raw_expr = unsafe {
            ffi::BinaryenLoad(
                self.inner.module,
                bytes,
                signed as i8,
                offset,
                align,
                ty.into(),
                ptr.into_raw(),
            )
        };
        Expr::from_raw(self, raw_expr)
    }

    pub fn store(
        &mut self,
        bytes: u32,
        offset: u32,
        align: u32,
        ptr: Expr,
        value: Expr,
        ty: ValueTy,
    ) -> Expr {
        let raw_expr = unsafe {
            ffi::BinaryenStore(
                self.inner.module,
                bytes,
                offset,
                align,
                ptr.into_raw(),
                value.into_raw(),
                ty.into(),
            )
        };
        Expr::from_raw(self, raw_expr)
    }

    pub fn get_global(&mut self, name: CString, ty: ValueTy) -> Expr {
        let global_name_ptr = self.save_string_and_return_ptr(name);
        let raw_expr =
            unsafe { ffi::BinaryenGetGlobal(self.inner.module, global_name_ptr, ty.into()) };
        Expr::from_raw(self, raw_expr)
    }

    pub fn set_global(&mut self, name: CString, value: Expr) -> Expr {
        let global_name_ptr = self.save_string_and_return_ptr(name);
        let raw_expr =
            unsafe { ffi::BinaryenSetGlobal(self.inner.module, global_name_ptr, value.into_raw()) };
        Expr::from_raw(self, raw_expr)
    }

    pub fn get_local(&mut self, index: u32, ty: ValueTy) -> Expr {
        let raw_expr = unsafe { 
            ffi::BinaryenGetLocal(self.inner.module, index as ffi::BinaryenIndex, ty.into()) 
        };
        Expr::from_raw(self, raw_expr)
    }

    pub fn set_local(&mut self, index: u32, value: Expr) -> Expr {
        let raw_expr = unsafe { 
            ffi::BinaryenSetLocal(self.inner.module, index as ffi::BinaryenIndex, value.into_raw()) 
        };
        Expr::from_raw(self, raw_expr)
    }

    pub fn tee_local(&mut self, index: u32, value: Expr) -> Expr {
        let raw_expr = unsafe { 
            ffi::BinaryenTeeLocal(self.inner.module, index as ffi::BinaryenIndex, value.into_raw()) 
        };
        Expr::from_raw(self, raw_expr)
    }

    pub fn ret(&mut self, value: Option<Expr>) -> Expr {
        let raw_expr = unsafe { 
            let raw_value = value.map_or(ptr::null_mut(), |v| v.into_raw());
            ffi::BinaryenReturn(self.inner.module, raw_value) 
        };
        Expr::from_raw(self, raw_expr)
    }

    pub fn call(&mut self, name: CString, operands: Vec<Expr>) -> Expr {
        let name_ptr = self.save_string_and_return_ptr(name);
        let raw_expr = unsafe {
            let mut operands_raw: Vec<_> = operands.into_iter().map(|ty| ty.into_raw()).collect();
            ffi::BinaryenCall(
                self.inner.module,
                name_ptr,
                operands_raw.as_mut_ptr(),
                operands_raw.len() as _,
                ffi::BinaryenNone(),
            )
        };
        Expr::from_raw(self, raw_expr)
    }

    pub fn call_import(&mut self, name: CString, operands: Vec<Expr>, ty: Ty) -> Expr {
        let name_ptr = self.save_string_and_return_ptr(name);
        let raw_expr = unsafe {
            let mut operands_raw: Vec<_> = operands.into_iter().map(|ty| ty.into_raw()).collect();
            ffi::BinaryenCallImport(
                self.inner.module,
                name_ptr,
                operands_raw.as_mut_ptr(),
                operands_raw.len() as _,
                ty.into(),
            )
        };
        Expr::from_raw(self, raw_expr)
    }

    pub fn binary(&mut self, op: BinaryOp, lhs: Expr, rhs: Expr) -> Expr {
        let raw_expr = unsafe {
            ffi::BinaryenBinary(
                self.inner.module, 
                op.into(), 
                lhs.into_raw(),
                rhs.into_raw()
            )
        };
        Expr::from_raw(self, raw_expr)
    }
}

pub enum BinaryOp {
    ClzInt32,
    CtzInt32,
    PopcntInt32,
    NegFloat32,
    AbsFloat32,
    CeilFloat32,
    FloorFloat32,
    TruncFloat32,
    NearestFloat32,
    SqrtFloat32,
    EqZInt32,
    ClzInt64,
    CtzInt64,
    PopcntInt64,
    NegFloat64,
    AbsFloat64,
    CeilFloat64,
    FloorFloat64,
    TruncFloat64,
    NearestFloat64,
    SqrtFloat64,
    EqZInt64,
    ExtendSInt32,
    ExtendUInt32,
    WrapInt64,
    TruncSFloat32ToInt32,
    TruncSFloat32ToInt64,
    TruncUFloat32ToInt32,
    TruncUFloat32ToInt64,
    TruncSFloat64ToInt32,
    TruncSFloat64ToInt64,
    TruncUFloat64ToInt32,
    TruncUFloat64ToInt64,
    ReinterpretFloat32,
    ReinterpretFloat64,
    ConvertSInt32ToFloat32,
    ConvertSInt32ToFloat64,
    ConvertUInt32ToFloat32,
    ConvertUInt32ToFloat64,
    ConvertSInt64ToFloat32,
    ConvertSInt64ToFloat64,
    ConvertUInt64ToFloat32,
    ConvertUInt64ToFloat64,
    PromoteFloat32,
    DemoteFloat64,
    ReinterpretInt32,
    ReinterpretInt64,
    AddInt32,
    SubInt32,
    MulInt32,
    DivSInt32,
    DivUInt32,
    RemSInt32,
    RemUInt32,
    AndInt32,
    OrInt32,
    XorInt32,
    ShlInt32,
    ShrUInt32,
    ShrSInt32,
    RotLInt32,
    RotRInt32,
    EqInt32,
    NeInt32,
    LtSInt32,
    LtUInt32,
    LeSInt32,
    LeUInt32,
    GtSInt32,
    GtUInt32,
    GeSInt32,
    GeUInt32,
    AddInt64,
    SubInt64,
    MulInt64,
    DivSInt64,
    DivUInt64,
    RemSInt64,
    RemUInt64,
    AndInt64,
    OrInt64,
    XorInt64,
    ShlInt64,
    ShrUInt64,
    ShrSInt64,
    RotLInt64,
    RotRInt64,
    EqInt64,
    NeInt64,
    LtSInt64,
    LtUInt64,
    LeSInt64,
    LeUInt64,
    GtSInt64,
    GtUInt64,
    GeSInt64,
    GeUInt64,
    AddFloat32,
    SubFloat32,
    MulFloat32,
    DivFloat32,
    CopySignFloat32,
    MinFloat32,
    MaxFloat32,
    EqFloat32,
    NeFloat32,
    LtFloat32,
    LeFloat32,
    GtFloat32,
    GeFloat32,
    AddFloat64,
    SubFloat64,
    MulFloat64,
    DivFloat64,
    CopySignFloat64,
    MinFloat64,
    MaxFloat64,
    EqFloat64,
    NeFloat64,
    LtFloat64,
    LeFloat64,
    GtFloat64,
    GeFloat64,
}

impl From<BinaryOp> for ffi::BinaryenOp {
    fn from(binop: BinaryOp) -> ffi::BinaryenOp {
        use BinaryOp::*;
        unsafe {
            match binop {
                ClzInt32 => ffi::BinaryenClzInt32(),
                CtzInt32 => ffi::BinaryenCtzInt32(),
                PopcntInt32 => ffi::BinaryenPopcntInt32(),
                NegFloat32 => ffi::BinaryenNegFloat32(),
                AbsFloat32 => ffi::BinaryenAbsFloat32(),
                CeilFloat32 => ffi::BinaryenCeilFloat32(),
                FloorFloat32 => ffi::BinaryenFloorFloat32(),
                TruncFloat32 => ffi::BinaryenTruncFloat32(),
                NearestFloat32 => ffi::BinaryenNearestFloat32(),
                SqrtFloat32 => ffi::BinaryenSqrtFloat32(),
                EqZInt32 => ffi::BinaryenEqZInt32(),
                ClzInt64 => ffi::BinaryenClzInt64(),
                CtzInt64 => ffi::BinaryenCtzInt64(),
                PopcntInt64 => ffi::BinaryenPopcntInt64(),
                NegFloat64 => ffi::BinaryenNegFloat64(),
                AbsFloat64 => ffi::BinaryenAbsFloat64(),
                CeilFloat64 => ffi::BinaryenCeilFloat64(),
                FloorFloat64 => ffi::BinaryenFloorFloat64(),
                TruncFloat64 => ffi::BinaryenTruncFloat64(),
                NearestFloat64 => ffi::BinaryenNearestFloat64(),
                SqrtFloat64 => ffi::BinaryenSqrtFloat64(),
                EqZInt64 => ffi::BinaryenEqZInt64(),
                ExtendSInt32 => ffi::BinaryenExtendSInt32(),
                ExtendUInt32 => ffi::BinaryenExtendUInt32(),
                WrapInt64 => ffi::BinaryenWrapInt64(),
                TruncSFloat32ToInt32 => ffi::BinaryenTruncSFloat32ToInt32(),
                TruncSFloat32ToInt64 => ffi::BinaryenTruncSFloat32ToInt64(),
                TruncUFloat32ToInt32 => ffi::BinaryenTruncUFloat32ToInt32(),
                TruncUFloat32ToInt64 => ffi::BinaryenTruncUFloat32ToInt64(),
                TruncSFloat64ToInt32 => ffi::BinaryenTruncSFloat64ToInt32(),
                TruncSFloat64ToInt64 => ffi::BinaryenTruncSFloat64ToInt64(),
                TruncUFloat64ToInt32 => ffi::BinaryenTruncUFloat64ToInt32(),
                TruncUFloat64ToInt64 => ffi::BinaryenTruncUFloat64ToInt64(),
                ReinterpretFloat32 => ffi::BinaryenReinterpretFloat32(),
                ReinterpretFloat64 => ffi::BinaryenReinterpretFloat64(),
                ConvertSInt32ToFloat32 => ffi::BinaryenConvertSInt32ToFloat32(),
                ConvertSInt32ToFloat64 => ffi::BinaryenConvertSInt32ToFloat64(),
                ConvertUInt32ToFloat32 => ffi::BinaryenConvertUInt32ToFloat32(),
                ConvertUInt32ToFloat64 => ffi::BinaryenConvertUInt32ToFloat64(),
                ConvertSInt64ToFloat32 => ffi::BinaryenConvertSInt64ToFloat32(),
                ConvertSInt64ToFloat64 => ffi::BinaryenConvertSInt64ToFloat64(),
                ConvertUInt64ToFloat32 => ffi::BinaryenConvertUInt64ToFloat32(),
                ConvertUInt64ToFloat64 => ffi::BinaryenConvertUInt64ToFloat64(),
                PromoteFloat32 => ffi::BinaryenPromoteFloat32(),
                DemoteFloat64 => ffi::BinaryenDemoteFloat64(),
                ReinterpretInt32 => ffi::BinaryenReinterpretInt32(),
                ReinterpretInt64 => ffi::BinaryenReinterpretInt64(),
                AddInt32 => ffi::BinaryenAddInt32(),
                SubInt32 => ffi::BinaryenSubInt32(),
                MulInt32 => ffi::BinaryenMulInt32(),
                DivSInt32 => ffi::BinaryenDivSInt32(),
                DivUInt32 => ffi::BinaryenDivUInt32(),
                RemSInt32 => ffi::BinaryenRemSInt32(),
                RemUInt32 => ffi::BinaryenRemUInt32(),
                AndInt32 => ffi::BinaryenAndInt32(),
                OrInt32 => ffi::BinaryenOrInt32(),
                XorInt32 => ffi::BinaryenXorInt32(),
                ShlInt32 => ffi::BinaryenShlInt32(),
                ShrUInt32 => ffi::BinaryenShrUInt32(),
                ShrSInt32 => ffi::BinaryenShrSInt32(),
                RotLInt32 => ffi::BinaryenRotLInt32(),
                RotRInt32 => ffi::BinaryenRotRInt32(),
                EqInt32 => ffi::BinaryenEqInt32(),
                NeInt32 => ffi::BinaryenNeInt32(),
                LtSInt32 => ffi::BinaryenLtSInt32(),
                LtUInt32 => ffi::BinaryenLtUInt32(),
                LeSInt32 => ffi::BinaryenLeSInt32(),
                LeUInt32 => ffi::BinaryenLeUInt32(),
                GtSInt32 => ffi::BinaryenGtSInt32(),
                GtUInt32 => ffi::BinaryenGtUInt32(),
                GeSInt32 => ffi::BinaryenGeSInt32(),
                GeUInt32 => ffi::BinaryenGeUInt32(),
                AddInt64 => ffi::BinaryenAddInt64(),
                SubInt64 => ffi::BinaryenSubInt64(),
                MulInt64 => ffi::BinaryenMulInt64(),
                DivSInt64 => ffi::BinaryenDivSInt64(),
                DivUInt64 => ffi::BinaryenDivUInt64(),
                RemSInt64 => ffi::BinaryenRemSInt64(),
                RemUInt64 => ffi::BinaryenRemUInt64(),
                AndInt64 => ffi::BinaryenAndInt64(),
                OrInt64 => ffi::BinaryenOrInt64(),
                XorInt64 => ffi::BinaryenXorInt64(),
                ShlInt64 => ffi::BinaryenShlInt64(),
                ShrUInt64 => ffi::BinaryenShrUInt64(),
                ShrSInt64 => ffi::BinaryenShrSInt64(),
                RotLInt64 => ffi::BinaryenRotLInt64(),
                RotRInt64 => ffi::BinaryenRotRInt64(),
                EqInt64 => ffi::BinaryenEqInt64(),
                NeInt64 => ffi::BinaryenNeInt64(),
                LtSInt64 => ffi::BinaryenLtSInt64(),
                LtUInt64 => ffi::BinaryenLtUInt64(),
                LeSInt64 => ffi::BinaryenLeSInt64(),
                LeUInt64 => ffi::BinaryenLeUInt64(),
                GtSInt64 => ffi::BinaryenGtSInt64(),
                GtUInt64 => ffi::BinaryenGtUInt64(),
                GeSInt64 => ffi::BinaryenGeSInt64(),
                GeUInt64 => ffi::BinaryenGeUInt64(),
                AddFloat32 => ffi::BinaryenAddFloat32(),
                SubFloat32 => ffi::BinaryenSubFloat32(),
                MulFloat32 => ffi::BinaryenMulFloat32(),
                DivFloat32 => ffi::BinaryenDivFloat32(),
                CopySignFloat32 => ffi::BinaryenCopySignFloat32(),
                MinFloat32 => ffi::BinaryenMinFloat32(),
                MaxFloat32 => ffi::BinaryenMaxFloat32(),
                EqFloat32 => ffi::BinaryenEqFloat32(),
                NeFloat32 => ffi::BinaryenNeFloat32(),
                LtFloat32 => ffi::BinaryenLtFloat32(),
                LeFloat32 => ffi::BinaryenLeFloat32(),
                GtFloat32 => ffi::BinaryenGtFloat32(),
                GeFloat32 => ffi::BinaryenGeFloat32(),
                AddFloat64 => ffi::BinaryenAddFloat64(),
                SubFloat64 => ffi::BinaryenSubFloat64(),
                MulFloat64 => ffi::BinaryenMulFloat64(),
                DivFloat64 => ffi::BinaryenDivFloat64(),
                CopySignFloat64 => ffi::BinaryenCopySignFloat64(),
                MinFloat64 => ffi::BinaryenMinFloat64(),
                MaxFloat64 => ffi::BinaryenMaxFloat64(),
                EqFloat64 => ffi::BinaryenEqFloat64(),
                NeFloat64 => ffi::BinaryenNeFloat64(),
                LtFloat64 => ffi::BinaryenLtFloat64(),
                LeFloat64 => ffi::BinaryenLeFloat64(),
                GtFloat64 => ffi::BinaryenGtFloat64(),
                GeFloat64 => ffi::BinaryenGeFloat64(),
            }
        }
    }
}

// TODO: Host

pub struct FnType {
    inner: ffi::BinaryenFunctionTypeRef,
}

pub struct FnRef {
    inner: ffi::BinaryenFunctionRef,
}

/// Type of the values. These can be found on the stack and
/// in the local vars.
#[derive(Copy, Clone)]
pub enum ValueTy {
    I32,
    I64,
    F32,
    F64,
}

pub struct Ty(Option<ValueTy>);

impl Ty {
    pub fn none() -> Ty {
        Ty(None)
    }

    pub fn value(ty: ValueTy) -> Ty {
        Ty(Some(ty))
    }
}

impl From<ValueTy> for ffi::BinaryenType {
    fn from(ty: ValueTy) -> ffi::BinaryenType {
        unsafe {
            match ty {
                ValueTy::I32 => ffi::BinaryenInt32(),
                ValueTy::I64 => ffi::BinaryenInt64(),
                ValueTy::F32 => ffi::BinaryenFloat32(),
                ValueTy::F64 => ffi::BinaryenFloat64(),
            }
        }
    }
}

impl From<Ty> for ffi::BinaryenType {
    fn from(ty: Ty) -> ffi::BinaryenType {
        match ty.0 {
            Some(ty) => ty.into(),
            None => unsafe { ffi::BinaryenNone() },
        }
    }
}

pub struct Expr {
    _module_ref: Rc<InnerModule>,
    raw: ffi::BinaryenExpressionRef,
}

impl Expr {
    pub fn from_raw(module: &Module, raw: ffi::BinaryenExpressionRef) -> Expr {
        Expr {
            _module_ref: Rc::clone(&module.inner),
            raw,
        }
    }

    pub unsafe fn into_raw(self) -> ffi::BinaryenExpressionRef {
        self.raw
    }
}

pub enum Literal {
    I32(u32),
    I64(u64),
    F32(f32),
    F64(f64),
}

impl From<Literal> for ffi::BinaryenLiteral {
    fn from(literal: Literal) -> ffi::BinaryenLiteral {
        unsafe {
            match literal {
                Literal::I32(v) => ffi::BinaryenLiteralInt32(v as i32),
                Literal::I64(v) => ffi::BinaryenLiteralInt64(v as i64),
                Literal::F32(v) => ffi::BinaryenLiteralFloat32(v),
                Literal::F64(v) => ffi::BinaryenLiteralFloat64(v),
            }
        }
    }
}

#[derive(Copy, Clone)]
pub struct RelooperBlockId(usize);

pub struct Relooper {
    inner: ffi::RelooperRef,
    blocks: Vec<ffi::RelooperBlockRef>,
}

impl Relooper {
    pub fn new() -> Relooper {
        Relooper {
            inner: unsafe { ffi::RelooperCreate() },
            blocks: Vec::new(),
        }
    }

    pub fn add_block<'m>(&mut self, expr: Expr) -> RelooperBlockId {
        let inner = unsafe { ffi::RelooperAddBlock(self.inner, expr.raw) };
        let index = self.blocks.len();
        self.blocks.push(inner);

        RelooperBlockId(index)
    }

    pub fn render(self, module: &Module, entry: RelooperBlockId, label_helper: u32) -> Expr {
        let entry = self.blocks[entry.0];
        let inner = unsafe {
            ffi::RelooperRenderAndDispose(self.inner, entry, label_helper as _, module.inner.module)
        };
        Expr::from_raw(module, inner)
    }

    pub fn add_branch<'m>(
        &mut self,
        from: RelooperBlockId,
        to: RelooperBlockId,
        condition: Option<Expr>,
        code: Option<Expr>,
    ) {
        let from_block = self.blocks[from.0];
        let to_block = self.blocks[to.0];

        unsafe {
            let condition_ptr = condition.map_or(ptr::null_mut(), |e| e.raw);
            let code_ptr = code.map_or(ptr::null_mut(), |e| e.raw);
            ffi::RelooperAddBranch(from_block as _, to_block as _, condition_ptr, code_ptr)
        }
    }
}
