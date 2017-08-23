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

    pub fn builder<'a>(&'a self) -> Builder<'a> {
        Builder {
            module: self.module,
            c_strings: self.c_strings.clone(),
            _marker: PhantomData
        }
    }

    pub fn new_fn_type(&self, name: Option<CString>, ty: Option<ValueTy>, param_tys: Vec<ValueTy>) -> FnType {
        let inner = unsafe {
            let name_ptr = name.map_or(ptr::null(), |n| self.save_string_and_return_ptr(n));
            ffi::BinaryenAddFunctionType(
                self.module,
                name_ptr,
                
            )
        };
        FnType {
            inner
        }
    }

    fn save_string_and_return_ptr(&self, string: CString) -> *const c_char {
        self.c_strings.borrow_mut().push(string);
        string.as_ptr()
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

pub struct Builder<'a> {
    module: ffi::BinaryenModuleRef,
    c_strings: Rc<RefCell<Vec<CString>>>,
    _marker: PhantomData<&'a ()>
}

impl<'a> Builder<'a> {
    // TODO: undefined ty?
    // https://github.com/WebAssembly/binaryen/blob/master/src/binaryen-c.h#L272
    fn block(&mut self, name: CString, mut children: Vec<Expr<'a>>, ty: ValueTy) -> Expr<'a> {
        let name_ptr = name.as_ptr();
        self.c_strings.borrow_mut().push(name);

        let binaryen_expr = unsafe {
            ffi::BinaryenBlock(self.module, name_ptr, children.as_mut_ptr() as _, children.len() as _, ty.into())
        };
        Expr::from_raw(binaryen_expr)
    }
}

#[derive(Copy, Clone)]
pub enum ValueTy {
    I32,
    I64,
    F32,
    F64
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


