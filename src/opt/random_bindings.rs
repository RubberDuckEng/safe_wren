use crate::ffi::c_api::*;
use crate::wren::WrenVM as RustWrenVM;
use crate::wren::{ForeignClassMethods, ForeignMethodFn};
use std::ffi::{CStr, CString};
use std::os::raw::c_char;

extern "C" {
    fn wrenRandomSource() -> *const c_char;

    fn wrenRandomBindForeignClass(
        vm: *mut WrenVM,
        module: *const c_char,
        class_name: *const c_char,
    ) -> WrenForeignClassMethods;

    fn wrenRandomBindForeignMethod(
        vm: *mut WrenVM,
        class_name: *const c_char,
        is_static: bool,
        signature: *const c_char,
    ) -> WrenForeignMethodFn;
}

pub fn random_source() -> String {
    let source = unsafe { wrenRandomSource() };
    unsafe { CStr::from_ptr(source) }.to_str().unwrap().into()
}

pub fn random_bind_foreign_class(
    vm: &mut RustWrenVM,
    module_name: &str,
    class_name: &str,
) -> ForeignClassMethods {
    let c_module = CString::new(module_name).unwrap();
    let c_class = CString::new(class_name).unwrap();
    let c_methods =
        unsafe { wrenRandomBindForeignClass(to_c_vm(vm), c_module.as_ptr(), c_class.as_ptr()) };
    ForeignClassMethods::from_c(c_methods)
}

pub fn random_bind_foreign_method(
    vm: &mut RustWrenVM,
    class_name: &str,
    is_static: bool,
    signature: &str,
) -> ForeignMethodFn {
    let c_class = CString::new(class_name).unwrap();
    let c_signature = CString::new(signature).unwrap();
    let c_method_fn = unsafe {
        wrenRandomBindForeignMethod(
            to_c_vm(vm),
            c_class.as_ptr(),
            is_static,
            c_signature.as_ptr(),
        )
    };
    unsafe { std::mem::transmute::<WrenForeignMethodFn, ForeignMethodFn>(c_method_fn) }
}
