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

    pub fn print(&mut self) {
        unsafe {
            ffi::BinaryenModulePrint(self.module)
        }
    }

    pub fn set_start(&mut self, fn_ref: &FnRef) {
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
    module: ffi::BinaryenModuleRef,
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

pub struct Expr<'a> {
    inner: ffi::BinaryenExpressionRef,
    _phantom: PhantomData<&'a ()>
}

impl<'a> Expr<'a> {
    pub fn from_raw<'b>(raw: ffi::BinaryenExpressionRef) -> Expr<'b> {
        Expr {
            inner: raw,
            _phantom: PhantomData
        }
    }
}

// pub struct Relooper<'a> {
//     inner: ffi::RelooperRef,
//     _phantom: PhantomData<&'a ()>
// }

// impl<'a> Relooper<'a> {
//     pub fn add_block(&mut self, expr: Expr) -> RelooperBlock {

//     }

//     pub fn render(self, module: Module, entry: RelooperBlock, label_helper: u32) -> Expr {
//     }
// }

// pub struct RelooperBlock<'a> {
//     pub fn add_branch(&mut self, to: &RelooperBlock, condition: Option<Expr>, code: Option<Expr>) {

//     }
// }


