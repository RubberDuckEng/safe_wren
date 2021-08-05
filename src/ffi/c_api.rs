use crate::vm::Value;
use crate::wren::{
    Configuration, FinalizerFn, ForeignClassMethods, ForeignMethodFn, InterpretResult, SlotType,
    UserData, VM,
};
use libc::{c_char, c_int, c_uint, size_t};
use std::boxed::Box;
use std::convert::TryFrom;
use std::ffi::{c_void, CStr};

#[repr(C)]
pub struct WrenVM {
    _unused: usize,
}

#[repr(C)]
pub struct WrenHandle {
    _unused: usize,
}

pub type WrenReallocateFn = Option<
    unsafe extern "C" fn(
        memory: *mut c_void,
        newSize: size_t,
        userData: *mut c_void,
    ) -> *mut c_void,
>;

pub type WrenForeignMethodFn = Option<unsafe extern "C" fn(vm: *mut WrenVM)>;
pub type WrenFinalizerFn = Option<unsafe extern "C" fn(data: *mut c_void)>;
pub type WrenResolveModuleFn = Option<
    unsafe extern "C" fn(
        vm: *mut WrenVM,
        importer: *const c_char,
        name: *const c_char,
    ) -> *const c_char,
>;
pub type WrenLoadModuleCompleteFn = Option<
    unsafe extern "C" fn(vm: *mut WrenVM, name: *const c_char, result: WrenLoadModuleResult),
>;

pub type WrenLoadModuleFn =
    Option<unsafe extern "C" fn(vm: *mut WrenVM, name: *const c_char) -> WrenLoadModuleResult>;
pub type WrenBindForeignMethodFn = Option<
    unsafe extern "C" fn(
        vm: *mut WrenVM,
        module: *const c_char,
        className: *const c_char,
        isStatic: bool,
        signature: *const c_char,
    ) -> WrenForeignMethodFn,
>;
pub type WrenWriteFn = Option<unsafe extern "C" fn(vm: *mut WrenVM, text: *const c_char)>;

pub type WrenBindForeignClassFn = Option<
    unsafe extern "C" fn(
        vm: *mut WrenVM,
        module: *const c_char,
        className: *const c_char,
    ) -> WrenForeignClassMethods,
>;

// #[allow(non_upper_case_globals)]
// pub const WrenErrorType_WREN_ERROR_COMPILE: WrenErrorType = 0;
// #[allow(non_upper_case_globals)]
// pub const WrenErrorType_WREN_ERROR_RUNTIME: WrenErrorType = 1;
// #[allow(non_upper_case_globals)]
// pub const WrenErrorType_WREN_ERROR_STACK_TRACE: WrenErrorType = 2;
pub type WrenErrorType = c_uint;

pub type WrenErrorFn = Option<
    unsafe extern "C" fn(
        vm: *mut WrenVM,
        type_: WrenErrorType,
        module: *const c_char,
        line: c_int,
        message: *const c_char,
    ),
>;

#[repr(C)]
pub struct WrenConfiguration {
    pub reallocate_fn: WrenReallocateFn, // ignored
    pub resolve_module_fn: WrenResolveModuleFn,
    pub load_module_fn: WrenLoadModuleFn,
    pub bind_foreign_method_fn: WrenBindForeignMethodFn,
    pub bind_foreign_class_fn: WrenBindForeignClassFn,
    pub write_fn: WrenWriteFn,
    pub error_fn: WrenErrorFn,
    pub initial_heap_size: size_t,  // ignored
    pub min_heap_size: size_t,      // ignored
    pub heap_growth_percent: c_int, // ignored
    pub user_data: *mut c_void,
}

#[repr(C)]
pub struct WrenLoadModuleResult {
    pub source: *const c_char,
    pub on_complete: WrenLoadModuleCompleteFn,
    pub user_data: *mut c_void,
}

#[repr(C)]
#[derive(Debug)]
pub struct WrenForeignClassMethods {
    pub allocate: WrenForeignMethodFn,
    pub finalize: WrenFinalizerFn,
}

#[no_mangle]
pub extern "C" fn wrenInitConfiguration(configuration: *mut WrenConfiguration) {
    let config = unsafe { &mut *configuration };
    config.reallocate_fn = None; // ignored
    config.resolve_module_fn = None;
    config.load_module_fn = None;
    config.bind_foreign_method_fn = None;
    config.bind_foreign_class_fn = None;
    config.write_fn = None;
    config.error_fn = None;
    config.initial_heap_size = 0; // ignored
    config.min_heap_size = 0; // ignored
    config.heap_growth_percent = 0; // ignored
    config.user_data = std::ptr::null_mut();
}

#[allow(non_upper_case_globals)]
pub const WrenInterpretResult_WREN_RESULT_SUCCESS: WrenInterpretResult = 0;
#[allow(non_upper_case_globals)]
pub const WrenInterpretResult_WREN_RESULT_COMPILE_ERROR: WrenInterpretResult = 1;
#[allow(non_upper_case_globals)]
pub const WrenInterpretResult_WREN_RESULT_RUNTIME_ERROR: WrenInterpretResult = 2;
pub type WrenInterpretResult = c_uint;

impl Configuration {
    pub fn from_c(c_config: &WrenConfiguration) -> Configuration {
        // c_config.resolve_module_fn: WrenResolveModuleFn,
        // c_config.load_module_fn: WrenLoadModuleFn,
        // c_config.bind_foreign_method_fn: WrenBindForeignMethodFn,
        // c_config.bind_foreign_class_fn: WrenBindForeignClassFn,
        // c_config.write_fn: WrenWriteFn,
        // c_config.error_fn: WrenErrorFn,
        // c_config.initial_heap_size: size_t,  // ignored
        // c_config.min_heap_size: size_t,      // ignored
        // c_config.heap_growth_percent: c_int, // ignored
        // c_config.user_data: *mut c_void,
        assert_eq!(c_config.reallocate_fn, None); // Ignored
        Configuration {
            ..Default::default()
        }
    }
}

#[no_mangle]
pub extern "C" fn wrenNewVM(c_config_ptr: *mut WrenConfiguration) -> *mut WrenVM {
    let c_config = unsafe { &*c_config_ptr };
    let config = Configuration::from_c(c_config);
    let mut vm = VM::new(config);
    vm.user_data = c_config.user_data;
    let vm_ptr = Box::into_raw(Box::new(vm));
    unsafe { std::mem::transmute::<*mut VM, *mut WrenVM>(vm_ptr) }
}

#[no_mangle]
pub extern "C" fn wrenFreeVM(c_vm: *mut WrenVM) {
    let vm_ptr = unsafe { std::mem::transmute::<*mut WrenVM, *mut VM>(c_vm) };
    unsafe { Box::from_raw(vm_ptr) };
}

impl InterpretResult {
    fn to_c(self) -> WrenInterpretResult {
        match self {
            InterpretResult::Success => WrenInterpretResult_WREN_RESULT_SUCCESS,
            InterpretResult::CompileError => WrenInterpretResult_WREN_RESULT_COMPILE_ERROR,
            InterpretResult::RuntimeError => WrenInterpretResult_WREN_RESULT_RUNTIME_ERROR,
        }
    }
}

#[no_mangle]
pub extern "C" fn wrenInterpret(
    c_vm: *mut WrenVM,
    c_module: *const c_char,
    c_source: *const c_char,
) -> WrenInterpretResult {
    let vm = unsafe { std::mem::transmute::<*mut WrenVM, &mut VM>(c_vm) };
    let module = unsafe { CStr::from_ptr(c_module) }.to_str().unwrap();
    let source = unsafe { CStr::from_ptr(c_source) }.to_str().unwrap().into();
    vm.interpret(module, source).to_c()
}

impl ForeignClassMethods {
    pub fn from_c(methods: WrenForeignClassMethods) -> ForeignClassMethods {
        let maybe_allocate = unsafe {
            std::mem::transmute::<WrenForeignMethodFn, Option<ForeignMethodFn>>(methods.allocate)
        };
        let maybe_finalize = unsafe {
            std::mem::transmute::<WrenFinalizerFn, Option<FinalizerFn>>(methods.finalize)
        };
        ForeignClassMethods {
            // allocate is required, even if the types don't say so.
            allocate: maybe_allocate.unwrap(),
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
    c_vm: *mut WrenVM,
    c_slot: c_int,
    c_class_slot: c_int,
    size: size_t,
) -> *mut c_void {
    let mut user_data = UserDataWrapper {
        buffer: vec![0; size].into_boxed_slice(),
    };
    let c_ptr = user_data.buffer.as_mut_ptr();
    let slot = usize::try_from(c_slot).unwrap();
    let class_slot = usize::try_from(c_class_slot).unwrap();

    let vm = unsafe { std::mem::transmute::<*mut WrenVM, &mut VM>(c_vm) };
    vm.set_slot_new_foreign(slot, class_slot, Box::new(user_data));
    unsafe { std::mem::transmute::<*mut u8, *mut c_void>(c_ptr) }
}

#[no_mangle]
extern "C" fn wrenMakeCallHandle(c_vm: *mut WrenVM, c_signature: *const c_char) -> *mut WrenHandle {
    let vm = unsafe { std::mem::transmute::<*mut WrenVM, &mut VM>(c_vm) };
    let signature = unsafe { CStr::from_ptr(c_signature) }.to_str().unwrap();
    let value = vm.call_handle_for_signature(signature);
    value.into_handle()
}

#[no_mangle]
extern "C" fn wrenCall(c_vm: *mut WrenVM, c_method: *mut WrenHandle) -> WrenInterpretResult {
    let vm = unsafe { std::mem::transmute::<*mut WrenVM, &mut VM>(c_vm) };
    let value = unsafe { std::mem::transmute::<*mut WrenHandle, &mut Value>(c_method) };
    // Copy api stack onto the (new?) real stack and then call the function?
    // What does it have access to?
    vm.call(value).to_c()
}

#[no_mangle]
extern "C" fn wrenReleaseHandle(_vm: *mut WrenVM, c_handle: *mut WrenHandle) {
    Value::from_handle(c_handle);
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

#[no_mangle]
pub extern "C" fn wrenGetSlotBytes(
    _vm: *mut WrenVM,
    _slot: c_int,
    _length: *mut c_int,
) -> *const c_char {
    // Is expected to live a long as the value its drawn from.
    std::ptr::null()
}

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

#[no_mangle]
pub extern "C" fn wrenGetSlotString(_vm: *mut WrenVM, _slot: c_int) -> *const c_char {
    // FIXME: unimplemented.
    std::ptr::null()
}

impl Value {
    fn into_handle(&self) -> *mut WrenHandle {
        let value_ptr = Box::into_raw(Box::new(self.clone()));
        unsafe { std::mem::transmute::<*mut Value, *mut WrenHandle>(value_ptr) }
    }

    fn from_handle(handle: *mut WrenHandle) -> Box<Value> {
        let value_ptr = unsafe { std::mem::transmute::<*mut WrenHandle, *mut Value>(handle) };
        unsafe { Box::from_raw(value_ptr) }
    }
}

#[no_mangle]
pub extern "C" fn wrenGetSlotHandle(c_vm: *mut WrenVM, c_slot: c_int) -> *mut WrenHandle {
    let vm = unsafe { std::mem::transmute::<*mut WrenVM, &mut VM>(c_vm) };
    let slot = usize::try_from(c_slot).unwrap();
    let value = vm.value_for_slot(slot);
    value.into_handle()
}

#[no_mangle]
pub extern "C" fn wrenSetSlotBool(c_vm: *mut WrenVM, c_slot: c_int, value: bool) {
    let vm = unsafe { std::mem::transmute::<*mut WrenVM, &mut VM>(c_vm) };
    let slot = usize::try_from(c_slot).unwrap();
    vm.set_slot_bool(slot, value)
}

#[no_mangle]
pub extern "C" fn wrenSetSlotBytes(
    _vm: *mut WrenVM,
    _slot: c_int,
    _bytes: *const c_char,
    _length: size_t,
) {
    // FIXME: rust strings don't take arbitrary byte arrays.
}

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

#[no_mangle]
pub extern "C" fn wrenSetSlotHandle(c_vm: *mut WrenVM, c_slot: c_int, c_handle: *mut WrenHandle) {
    let vm = unsafe { std::mem::transmute::<*mut WrenVM, &mut VM>(c_vm) };
    let value = unsafe { std::mem::transmute::<*mut WrenHandle, &mut Value>(c_handle) };
    let slot = usize::try_from(c_slot).unwrap();
    vm.set_value_for_slot(slot, value.clone());
}

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

#[no_mangle]
pub extern "C" fn wrenGetUserData(c_vm: *mut WrenVM) -> *mut c_void {
    let vm = unsafe { std::mem::transmute::<*mut WrenVM, &mut VM>(c_vm) };
    vm.user_data
}

#[no_mangle]
pub extern "C" fn wrenSetUserData(c_vm: *mut WrenVM, c_user_data: *mut c_void) {
    let vm = unsafe { std::mem::transmute::<*mut WrenVM, &mut VM>(c_vm) };
    vm.user_data = c_user_data;
}
