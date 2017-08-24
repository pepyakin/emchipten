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
            let mut param_tys_raw = param_tys.into_iter().map(|ty| ty.into()).collect::<Vec<_>>();
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

    fn save_string_and_return_ptr(&self, string: CString) -> *const c_char {
        let str_ptr = string.as_ptr();
        self.inner.c_strings.borrow_mut().push(string);
        str_ptr
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
                body.raw,
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
                init.raw,
            );
        }
    }

    // TODO: undefined ty?
    // https://github.com/WebAssembly/binaryen/blob/master/src/binaryen-c.h#L272
    pub fn block(&mut self, name: CString, mut children: Vec<Expr>, ty: Ty) -> Expr {
        let name_ptr = self.save_string_and_return_ptr(name);

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
}

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
