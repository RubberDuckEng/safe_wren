use crate::wren::WrenVM as RustWrenVM;
use crate::wren::{FinalizerFn, ForeignClassMethods, ForeignMethodFn, UserData};
use libc::{c_int, size_t};
use std::convert::TryFrom;
use std::ffi::c_void;

#[repr(C)]
pub struct WrenVM {
    _unused: usize,
}

// #[repr(C)]
// pub struct WrenHandle {
//     _unused: usize,
// }

#[repr(C)]
#[derive(Debug)]
pub struct WrenForeignClassMethods {
    pub allocate: Option<WrenForeignMethodFn>,
    pub finalize: Option<WrenFinalizerFn>,
}

// Called after loadModuleFn is called for module [name]. The original returned result
// is handed back to you in this callback, so that you can free memory if appropriate.
// pub type WrenLoadModuleCompleteFn =
//     unsafe extern "C" fn(_vm: *mut WrenVM, name: *const c_char, result: WrenLoadModuleResult);

// #[repr(C)]
// pub struct WrenLoadModuleResult {
//     pub source: *const c_char,
//     pub on_complete: WrenLoadModuleCompleteFn,
//     pub user_data: *mut c_void,
// }

pub type WrenForeignMethodFn = unsafe extern "C" fn(_vm: *mut WrenVM);
pub type WrenFinalizerFn = unsafe extern "C" fn(data: *mut c_void);

impl ForeignClassMethods {
    pub fn from_c(methods: WrenForeignClassMethods) -> ForeignClassMethods {
        let c_allocate = methods.allocate.unwrap();
        let allocate =
            unsafe { std::mem::transmute::<WrenForeignMethodFn, ForeignMethodFn>(c_allocate) };
        let maybe_finalize = if let Some(c_finalize) = methods.finalize {
            Some(unsafe { std::mem::transmute::<WrenFinalizerFn, FinalizerFn>(c_finalize) })
        } else {
            None
        };
        ForeignClassMethods {
            allocate: allocate,
            finalize: maybe_finalize,
        }
    }
}

pub fn to_c_vm(vm: &mut RustWrenVM) -> *mut WrenVM {
    unsafe { std::mem::transmute::<&mut RustWrenVM, *mut WrenVM>(vm) }
}

struct UserDataWrapper {
    buffer: Box<[u8]>,
}
impl UserData for UserDataWrapper {
    fn as_mut_any(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

#[no_mangle]
pub extern "C" fn wrenSetSlotNewForeign(
    vm: *mut WrenVM,
    slot: c_int,
    class_slot: c_int,
    size: size_t,
) -> *mut c_void {
    let mut user_data = UserDataWrapper {
        buffer: vec![0; size].into_boxed_slice(),
    };
    let c_ptr = user_data.buffer.as_mut_ptr();

    unsafe { std::mem::transmute::<*mut WrenVM, &mut RustWrenVM>(vm) }.set_slot_new_foreign(
        usize::try_from(slot).unwrap(),
        usize::try_from(class_slot).unwrap(),
        Box::new(user_data),
    );
    unsafe { std::mem::transmute::<*mut u8, *mut c_void>(c_ptr) }
}

// #[no_mangle]
// pub extern "C" fn wrenGetSlotCount(_vm: *mut WrenVM) -> c_int {
//     0
// }

// #[no_mangle]
// pub extern "C" fn wrenEnsureSlots(_vm: *mut WrenVM, _num_slots: c_int) {}

// #[repr(C)]
// #[allow(non_camel_case_types, dead_code)]
// pub enum WrenType {
//     WREN_TYPE_BOOL = 0,
//     WREN_TYPE_NUM = 1,
//     WREN_TYPE_FOREIGN = 2,
//     WREN_TYPE_LIST = 3,
//     WREN_TYPE_MAP = 4,
//     WREN_TYPE_NULL = 5,
//     WREN_TYPE_STRING = 6,
//     WREN_TYPE_UNKNOWN = 7,
// }

// #[no_mangle]
// pub extern "C" fn wrenGetSlotType(_vm: *mut WrenVM, _slot: c_int) -> WrenType {
//     WrenType::WREN_TYPE_UNKNOWN
// }

// #[no_mangle]
// pub extern "C" fn wrenGetSlotBool(_vm: *mut WrenVM, _slot: c_int) -> bool {
//     false
// }

// // #[no_mangle]
// // pub extern "C" fn  wrenGetSlotBytes(_vm: *mut WrenVM, _slot: c_int, length: *mut c_int) -> *c_char {

// // }

#[no_mangle]
pub extern "C" fn wrenGetSlotDouble(vm: *mut WrenVM, slot: c_int) -> f64 {
    unsafe { std::mem::transmute::<*mut WrenVM, &mut RustWrenVM>(vm) }
        .get_slot_double(usize::try_from(slot).unwrap())
}
#[no_mangle]
pub extern "C" fn wrenGetSlotForeign(c_vm: *mut WrenVM, c_slot: c_int) -> *mut c_void {
    let vm = unsafe { std::mem::transmute::<*mut WrenVM, &mut RustWrenVM>(c_vm) };
    let slot = usize::try_from(c_slot).unwrap();
    let foreign = vm.value_for_slot(slot).try_into_foreign().unwrap();
    let user_data = &mut foreign.borrow_mut().user_data;
    let c_user_data = user_data
        .as_mut_any()
        .downcast_mut::<UserDataWrapper>()
        .unwrap();
    let c_ptr = c_user_data.buffer.as_mut_ptr();
    unsafe { std::mem::transmute::<*mut u8, *mut c_void>(c_ptr) }
}

// #[no_mangle]
// pub extern "C" fn wrenGetSlotString(_vm: *mut WrenVM, _slot: c_int) -> *const c_char {
//     std::ptr::null()
// }

// #[no_mangle]
// pub extern "C" fn wrenGetSlotHandle(_vm: *mut WrenVM, _slot: c_int) -> *mut WrenHandle {
//     std::ptr::null_mut()
// }

// #[no_mangle]
// pub extern "C" fn wrenSetSlotBool(_vm: *mut WrenVM, _slot: c_int, _value: bool) {}

// #[no_mangle]
// pub extern "C" fn wrenSetSlotBytes(
//     _vm: *mut WrenVM,
//     _slot: c_int,
//     _bytes: *const c_char,
//     _length: size_t,
// ) {
// }

#[no_mangle]
pub extern "C" fn wrenSetSlotDouble(vm: *mut WrenVM, slot: c_int, value: f64) {
    unsafe { std::mem::transmute::<*mut WrenVM, &mut RustWrenVM>(vm) }
        .set_slot_double(usize::try_from(slot).unwrap(), value);
}

// #[no_mangle]
// pub extern "C" fn wrenSetSlotNewList(_vm: *mut WrenVM, _slot: c_int) {}

// #[no_mangle]
// pub extern "C" fn wrenSetSlotNewMap(_vm: *mut WrenVM, _slot: c_int) {}

// #[no_mangle]
// pub extern "C" fn wrenSetSlotNull(_vm: *mut WrenVM, _slot: c_int) {}

// #[no_mangle]
// pub extern "C" fn wrenSetSlotString(_vm: *mut WrenVM, _slot: c_int, _text: *const c_char) {}

// #[no_mangle]
// pub extern "C" fn wrenSetSlotHandle(_vm: *mut WrenVM, _slot: c_int, _handle: *mut WrenHandle) {}

// #[no_mangle]
// extern "C" fn wrenGetListCount(_vm: *mut WrenVM, _slot: c_int) -> c_int {
//     0
// }

// #[no_mangle]
// pub extern "C" fn wrenGetListElement(
//     _vm: *mut WrenVM,
//     _list_slot: c_int,
//     _index: c_int,
//     _element_slot: c_int,
// ) {
// }

// #[no_mangle]
// pub extern "C" fn wrenSetListElement(
//     _vm: *mut WrenVM,
//     _list_slot: c_int,
//     _index: c_int,
//     _element_slot: c_int,
// ) {
// }

// #[no_mangle]
// pub extern "C" fn wrenInsertInList(
//     _vm: *mut WrenVM,
//     _list_slot: c_int,
//     _index: c_int,
//     _element_slot: c_int,
// ) {
// }

// #[no_mangle]
// pub extern "C" fn wrenGetMapCount(_vm: *mut WrenVM, _slot: c_int) -> c_int {
//     0
// }

// #[no_mangle]
// pub extern "C" fn wrenGetMapContainsKey(
//     _vm: *mut WrenVM,
//     _map_slot: c_int,
//     _key_slot: c_int,
// ) -> bool {
//     false
// }

// #[no_mangle]
// pub extern "C" fn wrenGetMapValue(
//     _vm: *mut WrenVM,
//     _map_slot: c_int,
//     _key_slot: c_int,
//     _value_slot: c_int,
// ) {
// }

// #[no_mangle]
// pub extern "C" fn wrenSetMapValue(
//     _vm: *mut WrenVM,
//     _map_slot: c_int,
//     _key_slot: c_int,
//     _value_slot: c_int,
// ) {
// }

// #[no_mangle]
// pub extern "C" fn wrenRemoveMapValue(
//     _vm: *mut WrenVM,
//     _map_slot: c_int,
//     _key_slot: c_int,
//     _removed_value_slot: c_int,
// ) {
// }

// #[no_mangle]
// pub extern "C" fn wrenGetVariable(
//     _vm: *mut WrenVM,
//     _module: *const c_char,
//     _name: *const c_char,
//     _slot: c_int,
// ) {
// }

// #[no_mangle]
// pub extern "C" fn wrenHasVariable(
//     _vm: *mut WrenVM,
//     _module: *const c_char,
//     _name: *const c_char,
// ) -> bool {
//     false
// }

// #[no_mangle]
// pub extern "C" fn wrenHasModule(_vm: *mut WrenVM, _module: *const c_char) -> bool {
//     false
// }

// #[no_mangle]
// pub extern "C" fn wrenAbortFiber(_vm: *mut WrenVM, _slot: c_int) {}

// #[no_mangle]
// pub extern "C" fn wrenGetUserData(_vm: *mut WrenVM) -> *mut c_void {
//     std::ptr::null_mut()
// }

// #[no_mangle]
// pub extern "C" fn wrenSetUserData(_vm: *mut WrenVM, _user_data: *mut c_void) {}
