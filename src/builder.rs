use binaryen::ffi;
use std::marker::PhantomData;
use std::ffi::CString;
use std::cell::RefCell;
use std::rc::Rc;
use std::os::raw::c_char;
use std::ptr;

pub struct Module {
    pub module: ffi::BinaryenModuleRef,
    c_strings: Rc<RefCell<Vec<CString>>>,
}

impl Module {
    pub fn from_raw(module: ffi::BinaryenModuleRef) -> Module {
        Module {
            module,
            c_strings: Rc::new(RefCell::new(Vec::new()))
        }
    }

    pub fn auto_drop(&mut self) {
        unsafe {
            ffi::BinaryenModuleAutoDrop(self.module);
        }
    }

    pub fn optimize(&mut self) {
        unsafe { 
            ffi::BinaryenModuleOptimize(self.module) 
        }
    }

    pub fn is_valid(&mut self) -> bool {
        unsafe {
            ffi::BinaryenModuleValidate(self.module) == 1
        }
    }

    pub fn print(&self) {
        unsafe {
            ffi::BinaryenModulePrint(self.module)
        }
    }

    pub fn set_start(&mut self, fn_ref: &FnRef) {
        unsafe {
            ffi::BinaryenSetStart(self.module, fn_ref.inner);
        }
    }

    pub fn new_fn_type(&self, name: Option<CString>, ty: Ty, param_tys: Vec<ValueTy>) -> FnType {
        let inner = unsafe {
            let name_ptr = name.map_or(ptr::null(), |n| self.save_string_and_return_ptr(n));
            ffi::BinaryenAddFunctionType(
                self.module,
                name_ptr,
                ty.into(),
                param_tys.as_ptr() as _,
                param_tys.len() as _
            )
        };
        FnType {
            inner
        }
    }

    fn save_string_and_return_ptr(&self, string: CString) -> *const c_char {
        let str_ptr = string.as_ptr();
        self.c_strings.borrow_mut().push(string);
        str_ptr
    }

    pub fn new_fn<'a>(&'a self, name: CString, fn_ty: &FnType, var_tys: Vec<ValueTy>, body: Expr<'a>) -> FnRef {
        let inner = unsafe {
            let name_ptr = self.save_string_and_return_ptr(name);
            ffi::BinaryenAddFunction(
                self.module,
                name_ptr,
                fn_ty.inner,
                var_tys.as_ptr() as _,
                var_tys.len() as _,
                body.inner
            )
        };
        FnRef {
            inner
        }
    }

    // TODO: undefined ty?
    // https://github.com/WebAssembly/binaryen/blob/master/src/binaryen-c.h#L272
    pub fn block<'a>(&'a mut self, name: CString, mut children: Vec<Expr<'a>>, ty: Ty) -> Expr<'a> {
        let name_ptr = self.save_string_and_return_ptr(name);

        let binaryen_expr = unsafe {
            ffi::BinaryenBlock(self.module, name_ptr, children.as_mut_ptr() as _, children.len() as _, ty.into())
        };
        Expr::from_raw(binaryen_expr)
    }
}

impl Drop for Module {
    fn drop(&mut self) {
        unsafe { ffi::BinaryenModuleDispose(self.module) }
    }
}

pub struct FnType {
    inner: ffi::BinaryenFunctionTypeRef
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
    F64
}

pub struct Ty(Option<ValueTy>);

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
            None => unsafe {
                ffi::BinaryenNone()
            }
        }
    }
}

pub struct Expr<'m> {
    inner: ffi::BinaryenExpressionRef,
    _module: PhantomData<&'m ()>
}

impl<'m> Expr<'m> {
    pub fn from_raw<'b>(raw: ffi::BinaryenExpressionRef) -> Expr<'b> {
        Expr {
            inner: raw,
            _module: PhantomData
        }
    }

    pub unsafe fn into_raw(self) -> ffi::BinaryenExpressionRef {
        self.inner
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
            blocks: Vec::new()
        }
    }

    pub fn add_block<'m>(&mut self, expr: Expr<'m>) -> RelooperBlockId {
        let inner = unsafe {
            ffi::RelooperAddBlock(self.inner, expr.inner)
        };
        let index = self.blocks.len();
        self.blocks.push(inner);

        RelooperBlockId(index)
    }

    pub fn render<'m>(self, module: &'m Module, entry: RelooperBlockId, label_helper: u32) -> Expr<'m> {
        let entry = self.blocks[entry.0];
        let inner = unsafe {
            ffi::RelooperRenderAndDispose(self.inner, entry, label_helper as _, module.module)
        };
        Expr::from_raw(inner)
    }

    pub fn add_branch<'m>(&mut self, from: RelooperBlockId, to: RelooperBlockId, condition: Option<Expr<'m>>, code: Option<Expr<'m>>) {
        let from_block = self.blocks[from.0];
        let to_block = self.blocks[to.0];

        unsafe {
            let condition_ptr = condition.map_or(ptr::null_mut(), |e| e.inner);
            let code_ptr = code.map_or(ptr::null_mut(), |e| e.inner);
            ffi::RelooperAddBranch(from_block as _, to_block as _, condition_ptr, code_ptr)
        }
    }
}
