use crate::wren::{FinalizerFn, ForeignClassMethods, ForeignMethodFn, SlotType, UserData, VM};
use libc::{c_char, c_int, size_t};
use std::convert::TryFrom;
use std::ffi::{c_void, CStr};

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

pub fn to_c_vm(vm: &mut VM) -> *mut WrenVM {
    unsafe { std::mem::transmute::<&mut VM, *mut WrenVM>(vm) }
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

    unsafe { std::mem::transmute::<*mut WrenVM, &mut VM>(vm) }.set_slot_new_foreign(
        usize::try_from(slot).unwrap(),
        usize::try_from(class_slot).unwrap(),
        Box::new(user_data),
    );
    unsafe { std::mem::transmute::<*mut u8, *mut c_void>(c_ptr) }
}

#[no_mangle]
pub extern "C" fn wrenGetSlotCount(c_vm: *mut WrenVM) -> c_int {
    let vm = unsafe { std::mem::transmute::<*mut WrenVM, &mut VM>(c_vm) };
    let count = vm.slot_count();
    c_int::try_from(count).unwrap()
}

#[no_mangle]
pub extern "C" fn wrenEnsureSlots(c_vm: *mut WrenVM, num_slots: c_int) {
    let vm = unsafe { std::mem::transmute::<*mut WrenVM, &mut VM>(c_vm) };
    let num_slots = usize::try_from(num_slots).unwrap();
    vm.ensure_slots(num_slots)
}

#[repr(C)]
#[allow(non_camel_case_types, dead_code)]
pub enum WrenType {
    WREN_TYPE_BOOL = 0,
    WREN_TYPE_NUM = 1,
    WREN_TYPE_FOREIGN = 2,
    WREN_TYPE_LIST = 3,
    WREN_TYPE_MAP = 4,
    WREN_TYPE_NULL = 5,
    WREN_TYPE_STRING = 6,
    WREN_TYPE_UNKNOWN = 7,
}

#[no_mangle]
pub extern "C" fn wrenGetSlotType(c_vm: *mut WrenVM, c_slot: c_int) -> WrenType {
    let vm = unsafe { std::mem::transmute::<*mut WrenVM, &mut VM>(c_vm) };
    let slot = usize::try_from(c_slot).unwrap();
    match vm.type_for_slot(slot) {
        SlotType::Bool => WrenType::WREN_TYPE_BOOL,
        SlotType::Num => WrenType::WREN_TYPE_NUM,
        SlotType::Foreign => WrenType::WREN_TYPE_FOREIGN,
        SlotType::List => WrenType::WREN_TYPE_LIST,
        SlotType::Map => WrenType::WREN_TYPE_MAP,
        SlotType::Null => WrenType::WREN_TYPE_NULL,
        SlotType::String => WrenType::WREN_TYPE_STRING,
        SlotType::Unknown => WrenType::WREN_TYPE_UNKNOWN,
    }
}

#[no_mangle]
pub extern "C" fn wrenGetSlotBool(c_vm: *mut WrenVM, c_slot: c_int) -> bool {
    let vm = unsafe { std::mem::transmute::<*mut WrenVM, &mut VM>(c_vm) };
    let slot = usize::try_from(c_slot).unwrap();
    vm.get_slot_bool(slot)
}

// // #[no_mangle]
// // pub extern "C" fn  wrenGetSlotBytes(c_vm: *mut WrenVM, c_slot: c_int, length: *mut c_int) -> *c_char {

// // }

#[no_mangle]
pub extern "C" fn wrenGetSlotDouble(c_vm: *mut WrenVM, c_slot: c_int) -> f64 {
    let vm = unsafe { std::mem::transmute::<*mut WrenVM, &mut VM>(c_vm) };
    let slot = usize::try_from(c_slot).unwrap();
    vm.get_slot_double(slot)
}
#[no_mangle]
pub extern "C" fn wrenGetSlotForeign(c_vm: *mut WrenVM, c_slot: c_int) -> *mut c_void {
    let vm = unsafe { std::mem::transmute::<*mut WrenVM, &mut VM>(c_vm) };
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
// pub extern "C" fn wrenGetSlotString(c_vm: *mut WrenVM, c_slot: c_int) -> *const c_char {
//     std::ptr::null()
// }

// #[no_mangle]
// pub extern "C" fn wrenGetSlotHandle(c_vm: *mut WrenVM, c_slot: c_int) -> *mut WrenHandle {
//     std::ptr::null_mut()
// }

#[no_mangle]
pub extern "C" fn wrenSetSlotBool(c_vm: *mut WrenVM, c_slot: c_int, value: bool) {
    let vm = unsafe { std::mem::transmute::<*mut WrenVM, &mut VM>(c_vm) };
    let slot = usize::try_from(c_slot).unwrap();
    vm.set_slot_bool(slot, value)
}

// #[no_mangle]
// pub extern "C" fn wrenSetSlotBytes(
//     c_vm: *mut WrenVM,
//     c_slot: c_int,
//     c_bytes: *const c_char,
//     c_length: size_t,
// ) {
// }

#[no_mangle]
pub extern "C" fn wrenSetSlotDouble(c_vm: *mut WrenVM, c_slot: c_int, value: f64) {
    let vm = unsafe { std::mem::transmute::<*mut WrenVM, &mut VM>(c_vm) };
    let slot = usize::try_from(c_slot).unwrap();
    vm.set_slot_double(usize::try_from(slot).unwrap(), value);
}

#[no_mangle]
pub extern "C" fn wrenSetSlotNewList(c_vm: *mut WrenVM, c_slot: c_int) {
    let vm = unsafe { std::mem::transmute::<*mut WrenVM, &mut VM>(c_vm) };
    let slot = usize::try_from(c_slot).unwrap();
    vm.set_slot_new_list(slot);
}

#[no_mangle]
pub extern "C" fn wrenSetSlotNewMap(c_vm: *mut WrenVM, c_slot: c_int) {
    let vm = unsafe { std::mem::transmute::<*mut WrenVM, &mut VM>(c_vm) };
    let slot = usize::try_from(c_slot).unwrap();
    vm.set_slot_new_map(slot);
}

#[no_mangle]
pub extern "C" fn wrenSetSlotNull(c_vm: *mut WrenVM, c_slot: c_int) {
    let vm = unsafe { std::mem::transmute::<*mut WrenVM, &mut VM>(c_vm) };
    let slot = usize::try_from(c_slot).unwrap();
    vm.set_slot_null(slot);
}

#[no_mangle]
pub extern "C" fn wrenSetSlotString(c_vm: *mut WrenVM, c_slot: c_int, c_text: *const c_char) {
    let vm = unsafe { std::mem::transmute::<*mut WrenVM, &mut VM>(c_vm) };
    let slot = usize::try_from(c_slot).unwrap();
    let string = unsafe { CStr::from_ptr(c_text) }.to_str().unwrap().into();
    vm.set_slot_string(slot, string);
}

// #[no_mangle]
// pub extern "C" fn wrenSetSlotHandle(c_vm: *mut WrenVM, c_slot: c_int, _handle: *mut WrenHandle) {}

#[no_mangle]
extern "C" fn wrenGetListCount(c_vm: *mut WrenVM, c_slot: c_int) -> c_int {
    let vm = unsafe { std::mem::transmute::<*mut WrenVM, &mut VM>(c_vm) };
    let slot = usize::try_from(c_slot).unwrap();
    let count = vm.get_list_count(slot);
    c_int::try_from(count).unwrap()
}

#[no_mangle]
pub extern "C" fn wrenGetListElement(
    c_vm: *mut WrenVM,
    c_list_slot: c_int,
    c_index: c_int,
    c_element_slot: c_int,
) {
    let vm = unsafe { std::mem::transmute::<*mut WrenVM, &mut VM>(c_vm) };
    let list_slot = usize::try_from(c_list_slot).unwrap();
    let index = usize::try_from(c_index).unwrap();
    let element_slot = usize::try_from(c_element_slot).unwrap();
    vm.get_list_element(list_slot, index, element_slot)
}

#[no_mangle]
pub extern "C" fn wrenSetListElement(
    c_vm: *mut WrenVM,
    c_list_slot: c_int,
    c_index: c_int,
    c_element_slot: c_int,
) {
    let vm = unsafe { std::mem::transmute::<*mut WrenVM, &mut VM>(c_vm) };
    let list_slot = usize::try_from(c_list_slot).unwrap();
    let index = usize::try_from(c_index).unwrap();
    let element_slot = usize::try_from(c_element_slot).unwrap();
    vm.set_list_element(list_slot, index, element_slot)
}

#[no_mangle]
pub extern "C" fn wrenInsertInList(
    c_vm: *mut WrenVM,
    c_list_slot: c_int,
    c_index: c_int,
    c_element_slot: c_int,
) {
    let vm = unsafe { std::mem::transmute::<*mut WrenVM, &mut VM>(c_vm) };
    let list_slot = usize::try_from(c_list_slot).unwrap();
    let index = usize::try_from(c_index).unwrap();
    let element_slot = usize::try_from(c_element_slot).unwrap();
    vm.insert_in_list(list_slot, index, element_slot)
}

#[no_mangle]
pub extern "C" fn wrenGetMapCount(c_vm: *mut WrenVM, c_slot: c_int) -> c_int {
    let vm = unsafe { std::mem::transmute::<*mut WrenVM, &mut VM>(c_vm) };
    let slot = usize::try_from(c_slot).unwrap();
    let value = vm.value_for_slot(slot);
    let map = value.try_into_map().unwrap();
    let count = map.borrow().len();
    c_int::try_from(count).unwrap()
}

#[no_mangle]
pub extern "C" fn wrenGetMapContainsKey(
    c_vm: *mut WrenVM,
    c_map_slot: c_int,
    c_key_slot: c_int,
) -> bool {
    let vm = unsafe { std::mem::transmute::<*mut WrenVM, &mut VM>(c_vm) };
    let map_slot = usize::try_from(c_map_slot).unwrap();
    let key_slot = usize::try_from(c_key_slot).unwrap();
    vm.map_contains_key(map_slot, key_slot)
}

#[no_mangle]
pub extern "C" fn wrenGetMapValue(
    c_vm: *mut WrenVM,
    c_map_slot: c_int,
    c_key_slot: c_int,
    c_value_slot: c_int,
) {
    let vm = unsafe { std::mem::transmute::<*mut WrenVM, &mut VM>(c_vm) };
    let map_slot = usize::try_from(c_map_slot).unwrap();
    let key_slot = usize::try_from(c_key_slot).unwrap();
    let value_slot = usize::try_from(c_value_slot).unwrap();
    vm.get_map_value(map_slot, key_slot, value_slot)
}

#[no_mangle]
pub extern "C" fn wrenSetMapValue(
    c_vm: *mut WrenVM,
    c_map_slot: c_int,
    c_key_slot: c_int,
    c_value_slot: c_int,
) {
    let vm = unsafe { std::mem::transmute::<*mut WrenVM, &mut VM>(c_vm) };
    let map_slot = usize::try_from(c_map_slot).unwrap();
    let key_slot = usize::try_from(c_key_slot).unwrap();
    let value_slot = usize::try_from(c_value_slot).unwrap();
    vm.set_map_value(map_slot, key_slot, value_slot)
}

#[no_mangle]
pub extern "C" fn wrenRemoveMapValue(
    c_vm: *mut WrenVM,
    c_map_slot: c_int,
    c_key_slot: c_int,
    c_removed_value_slot: c_int,
) {
    let vm = unsafe { std::mem::transmute::<*mut WrenVM, &mut VM>(c_vm) };
    let map_slot = usize::try_from(c_map_slot).unwrap();
    let key_slot = usize::try_from(c_key_slot).unwrap();
    let removed_value_slot = usize::try_from(c_removed_value_slot).unwrap();
    vm.remove_map_value(map_slot, key_slot, removed_value_slot)
}

#[no_mangle]
pub extern "C" fn wrenGetVariable(
    c_vm: *mut WrenVM,
    c_module: *const c_char,
    c_name: *const c_char,
    c_slot: c_int,
) {
    let vm = unsafe { std::mem::transmute::<*mut WrenVM, &mut VM>(c_vm) };
    let module_name = unsafe { CStr::from_ptr(c_module) }.to_str().unwrap();
    let variable_name = unsafe { CStr::from_ptr(c_name) }.to_str().unwrap();
    let slot = usize::try_from(c_slot).unwrap();
    vm.get_variable(module_name, variable_name, slot)
}

#[no_mangle]
pub extern "C" fn wrenHasVariable(
    c_vm: *mut WrenVM,
    c_module: *const c_char,
    c_name: *const c_char,
) -> bool {
    let vm = unsafe { std::mem::transmute::<*mut WrenVM, &mut VM>(c_vm) };
    let module_name = unsafe { CStr::from_ptr(c_module) }.to_str().unwrap();
    let variable_name = unsafe { CStr::from_ptr(c_name) }.to_str().unwrap();
    vm.has_variable(module_name, variable_name)
}

#[no_mangle]
pub extern "C" fn wrenHasModule(c_vm: *mut WrenVM, c_module: *const c_char) -> bool {
    let vm = unsafe { std::mem::transmute::<*mut WrenVM, &mut VM>(c_vm) };
    let module = unsafe { CStr::from_ptr(c_module) }.to_str().unwrap();
    vm.has_module(module)
}

#[no_mangle]
pub extern "C" fn wrenAbortFiber(c_vm: *mut WrenVM, c_slot: c_int) {
    let vm = unsafe { std::mem::transmute::<*mut WrenVM, &mut VM>(c_vm) };
    let slot = usize::try_from(c_slot).unwrap();
    vm.abort_fiber(slot)
}

// #[no_mangle]
// pub extern "C" fn wrenGetUserData(c_vm: *mut WrenVM) -> *mut c_void {
//     std::ptr::null_mut()
// }

// #[no_mangle]
// pub extern "C" fn wrenSetUserData(c_vm: *mut WrenVM, c_user_data: *mut c_void) {}
