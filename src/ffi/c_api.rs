// FIXME: Sit on top of the rust API and not include private crates (e.g. vm)?
use crate::vm::*;
use crate::wren::*;
use libc::{c_char, c_int, c_uint, size_t};
use std::boxed::Box;
use std::convert::TryFrom;
use std::ffi::{c_void, CStr, CString};
use vmgc::*;

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

#[allow(non_upper_case_globals)]
pub const WrenErrorType_WREN_ERROR_COMPILE: WrenErrorType = 0;
#[allow(non_upper_case_globals)]
pub const WrenErrorType_WREN_ERROR_RUNTIME: WrenErrorType = 1;
#[allow(non_upper_case_globals)]
pub const WrenErrorType_WREN_ERROR_STACK_TRACE: WrenErrorType = 2;
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
#[derive(Clone)]
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

impl Default for WrenConfiguration {
    fn default() -> WrenConfiguration {
        WrenConfiguration {
            reallocate_fn: None,
            resolve_module_fn: None,
            load_module_fn: None,
            bind_foreign_method_fn: None,
            bind_foreign_class_fn: None,
            write_fn: None,
            error_fn: None,
            initial_heap_size: 0,
            min_heap_size: 0,
            heap_growth_percent: 0,
            user_data: std::ptr::null_mut(),
        }
    }
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

impl ErrorType {
    fn to_c(self) -> WrenErrorType {
        match self {
            ErrorType::Compile => WrenErrorType_WREN_ERROR_COMPILE,
            ErrorType::Runtime => WrenErrorType_WREN_ERROR_RUNTIME,
            ErrorType::StackTrace => WrenErrorType_WREN_ERROR_STACK_TRACE,
        }
    }
}

impl Configuration {
    pub fn from_c(c_config: &WrenConfiguration) -> Configuration {
        // We have to hold onto the function pointer from C somehow.
        // We could use closures and capture the fn pointer.  But for now we're
        // just saving the whole c_config struct and grabbing the function
        // pointers off of that when dispatching to these static rust functions.
        // If we ever change Configuration (rust) to take Closures, we could
        // switch to using Closures instead.
        fn write_fn(vm: &VM, text: &str) {
            let c_fn = vm.c_config.write_fn.unwrap();
            let c_vm = to_const_c_vm(vm);
            let c_text = CString::new(text).unwrap();
            unsafe { c_fn(c_vm, c_text.as_ptr()) };
        }
        fn resolve_module_fn(vm: &VM, importer: &str, name: &str) -> String {
            let c_fn = vm.c_config.resolve_module_fn.unwrap();
            let c_vm = to_const_c_vm(vm);
            let c_importer = CString::new(importer).unwrap();
            let c_name = CString::new(name).unwrap();
            let c_resolved = unsafe { c_fn(c_vm, c_importer.as_ptr(), c_name.as_ptr()) };
            let resolved = unsafe { CStr::from_ptr(c_resolved) }
                .to_str()
                .unwrap()
                .into();
            // FIXME: LEAK.  This should config.realloc to free.
            // crate::libc::free(c_resolved); // Wrong mutability.
            resolved
        }
        fn error_fn(vm: &VM, error_type: ErrorType, module: &str, line: usize, message: &str) {
            let c_fn = vm.c_config.error_fn.unwrap();
            let c_vm = to_const_c_vm(vm);
            let c_error_type = error_type.to_c();
            let c_module = CString::new(module).unwrap();
            let c_line = c_int::try_from(line).unwrap();
            let c_message = CString::new(message).unwrap();
            unsafe {
                c_fn(
                    c_vm,
                    c_error_type,
                    c_module.as_ptr(),
                    c_line,
                    c_message.as_ptr(),
                )
            };
        }

        fn load_module_fn(vm: &VM, name: &str) -> Option<LoadModuleResult> {
            let c_fn = vm.c_config.load_module_fn.unwrap();
            let c_vm = to_const_c_vm(vm);
            let c_name = CString::new(name).unwrap();
            let c_result = unsafe { c_fn(c_vm, c_name.as_ptr()) };
            if c_result.source.is_null() {
                // FIXME: call the completion function?
                return None;
            }
            let source = unsafe { CStr::from_ptr(c_result.source) }
                .to_str()
                .unwrap()
                .into();
            Some(LoadModuleResult { source })
        }

        fn bind_foreign_method_fn(
            vm: &VM,
            module: &str,
            class_name: &str,
            is_static: bool,
            signature: &str,
        ) -> Option<ForeignMethodFn> {
            let c_fn = vm.c_config.bind_foreign_method_fn.unwrap();
            let c_vm = to_const_c_vm(vm);
            let c_module = CString::new(module).unwrap();
            let c_class_name = CString::new(class_name).unwrap();
            let c_signature = CString::new(signature).unwrap();
            let c_foreign_method_fn = unsafe {
                c_fn(
                    c_vm,
                    c_module.as_ptr(),
                    c_class_name.as_ptr(),
                    is_static,
                    c_signature.as_ptr(),
                )
            };
            type InnerFn = unsafe extern "C" fn(vm: *mut WrenVM);
            // Same signature, so direct transmute is possible.
            c_foreign_method_fn
                .map(|func| unsafe { std::mem::transmute::<InnerFn, ForeignMethodFn>(func) })
        }

        fn bind_foreign_class_fn(
            vm: &VM,
            module_name: &str,
            class_name: &str,
        ) -> ForeignClassMethods {
            let c_fn = vm.c_config.bind_foreign_class_fn.unwrap();
            let c_vm = to_const_c_vm(vm);
            let c_module = CString::new(module_name).unwrap();
            let c_class_name = CString::new(class_name).unwrap();
            let c_foreign_class_methods =
                unsafe { c_fn(c_vm, c_module.as_ptr(), c_class_name.as_ptr()) };
            ForeignClassMethods::from_c(c_foreign_class_methods)
        }

        // c_config.initial_heap_size: size_t,  // ignored, no GC
        // c_config.min_heap_size: size_t,      // ignored, no GC
        // c_config.heap_growth_percent: c_int, // ignored, no GC
        // c_config.user_data: *mut c_void, // handled outside.
        // c_config.reallocate_fn // FIXME: Need to add support.
        Configuration {
            // FIXME: is "as WriteFn" really needed?
            // https://users.rust-lang.org/t/puzzling-expected-fn-pointer-found-fn-item/46423/4
            write_fn: c_config.write_fn.map(|_| write_fn as WriteFn),
            resolve_module_fn: c_config
                .resolve_module_fn
                .map(|_| resolve_module_fn as ResolveModuleFn),
            error_fn: c_config.error_fn.map(|_| error_fn as ErrorFn),
            load_module_fn: c_config
                .load_module_fn
                .map(|_| load_module_fn as LoadModuleFn),
            bind_foreign_method_fn: c_config
                .bind_foreign_method_fn
                .map(|_| bind_foreign_method_fn as BindForeignMethodFn),
            bind_foreign_class_fn: c_config
                .bind_foreign_class_fn
                .map(|_| bind_foreign_class_fn as BindForeignClassFn),
            ..Default::default()
        }
    }
}

// FIXME: Should API or c_config move onto CallContext instead of VM?
struct CallContext<'vm> {
    vm_and_heap_ptr: *mut VMAndHeap,
    vm: &'vm mut VM,
    // FIXME: Is this lifetime correct?
    scope: HandleScope<'vm>,
}

impl<'vm> CallContext<'vm> {
    fn to_c_vm(self) -> *mut WrenVM {
        unsafe { std::mem::transmute::<*mut VMAndHeap, *mut WrenVM>(self.vm_and_heap_ptr) }
    }

    fn from_c_vm(c_vm: *mut WrenVM) -> Self {
        let vm_and_heap_ptr = unsafe { std::mem::transmute::<*mut WrenVM, *mut VMAndHeap>(c_vm) };
        let vm_and_heap =
            unsafe { std::mem::transmute::<*mut VMAndHeap, &mut VMAndHeap>(vm_and_heap_ptr) };
        CallContext {
            vm_and_heap_ptr: vm_and_heap_ptr,
            vm: &mut vm_and_heap.vm,
            scope: HandleScope::new(&vm_and_heap.heap),
        }
    }
}

#[no_mangle]
pub extern "C" fn wrenNewVM(c_config_ptr: *mut WrenConfiguration) -> *mut WrenVM {
    let c_config = unsafe { &*c_config_ptr };
    let config = Configuration::from_c(c_config);
    let vm_and_heap = VMAndHeap::new(config);
    // Also hold onto the config for the function pointers.
    vm_and_heap.vm.c_config = c_config.clone();
    let vm_and_heap_ptr = Box::into_raw(Box::new(vm_and_heap));
    unsafe { std::mem::transmute::<*mut VMAndHeap, *mut WrenVM>(vm_and_heap_ptr) }
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

fn to_c_vm(vm: &mut VM) -> *mut WrenVM {
    unsafe { std::mem::transmute::<&mut VM, *mut WrenVM>(vm) }
}

// FIXME: This is extra-unsafe.  It's used when the C API callbacks are
// declared with mutable WrenVM*, but the rust-API uses a &VM.
fn to_const_c_vm(vm: &VM) -> *mut WrenVM {
    unsafe { std::mem::transmute::<&VM, *mut WrenVM>(vm) }
}

macro_rules! from_c_vm {
    ($a:expr) => {
        CallContext::from_c_vm($a)
        // unsafe { std::mem::transmute::<*mut WrenVM, &mut VM>($a) }
    };
}

#[no_mangle]
pub extern "C" fn wrenInterpret(
    c_vm: *mut WrenVM,
    c_module: *const c_char,
    c_source: *const c_char,
) -> WrenInterpretResult {
    let vm = from_c_vm!(c_vm);
    let module = unsafe { CStr::from_ptr(c_module) }.to_str().unwrap();
    let source = unsafe { CStr::from_ptr(c_source) };
    vm.vm
        .interpret_bytes(&vm.scope, module, source.to_bytes().into())
        .to_c()
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

    let vm = from_c_vm!(c_vm);
    vm.vm
        .set_slot_new_foreign(&vm.scope, slot, class_slot, Box::new(user_data));
    unsafe { std::mem::transmute::<*mut u8, *mut c_void>(c_ptr) }
}

// #[no_mangle]
// extern "C" fn wrenMakeCallHandle(c_vm: *mut WrenVM, c_signature: *const c_char) -> *mut WrenHandle {
//     let vm = from_c_vm!(c_vm);
//     let signature = unsafe { CStr::from_ptr(c_signature) }.to_str().unwrap();
//     let value = vm.vm.call_handle_for_signature(&vm.scope, signature);
//     value.into_handle()
// }

// #[no_mangle]
// extern "C" fn wrenCall(c_vm: *mut WrenVM, c_method: *mut WrenHandle) -> WrenInterpretResult {
//     let vm = from_c_vm!(c_vm);
//     let value = unsafe { std::mem::transmute::<*mut WrenHandle, &mut Value>(c_method) };
//     // Copy api stack onto the (new?) real stack and then call the function?
//     // What does it have access to?
//     vm.call(value).to_c()
// }

// #[no_mangle]
// extern "C" fn wrenReleaseHandle(_vm: *mut WrenVM, c_handle: *mut WrenHandle) {
//     Value::from_handle(c_handle);
// }

#[no_mangle]
pub extern "C" fn wrenGetSlotCount(c_vm: *mut WrenVM) -> c_int {
    let vm = from_c_vm!(c_vm);
    let count = vm.vm.slot_count(&vm.scope);
    c_int::try_from(count).unwrap()
}

#[no_mangle]
pub extern "C" fn wrenEnsureSlots(c_vm: *mut WrenVM, num_slots: c_int) {
    let vm = from_c_vm!(c_vm);
    let num_slots = usize::try_from(num_slots).unwrap();
    vm.vm.ensure_slots(&vm.scope, num_slots)
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
    let vm = from_c_vm!(c_vm);
    let slot = usize::try_from(c_slot).unwrap();
    match vm.vm.type_for_slot(&vm.scope, slot) {
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
    let vm = from_c_vm!(c_vm);
    let slot = usize::try_from(c_slot).unwrap();
    vm.vm.get_slot_bool(&vm.scope, slot)
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
    let vm = from_c_vm!(c_vm);
    let slot = usize::try_from(c_slot).unwrap();
    vm.vm.get_slot_double(&vm.scope, slot)
}
#[no_mangle]
pub extern "C" fn wrenGetSlotForeign(c_vm: *mut WrenVM, c_slot: c_int) -> *mut c_void {
    let vm = from_c_vm!(c_vm);
    let slot = usize::try_from(c_slot).unwrap();
    let foreign: LocalHandle<ObjForeign> = vm
        .vm
        .value_for_slot(&vm.scope, slot)
        .try_downcast()
        .unwrap();
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

// impl Value {
//     fn into_handle(&self) -> *mut WrenHandle {
//         let value_ptr = Box::into_raw(Box::new(self.clone()));
//         unsafe { std::mem::transmute::<*mut Value, *mut WrenHandle>(value_ptr) }
//     }

//     fn from_handle(handle: *mut WrenHandle) -> Box<Value> {
//         let value_ptr = unsafe { std::mem::transmute::<*mut WrenHandle, *mut Value>(handle) };
//         unsafe { Box::from_raw(value_ptr) }
//     }
// }

// #[no_mangle]
// pub extern "C" fn wrenGetSlotHandle(c_vm: *mut WrenVM, c_slot: c_int) -> *mut WrenHandle {
//     let vm = from_c_vm!(c_vm);
//     let slot = usize::try_from(c_slot).unwrap();
//     let value = vm.vm.value_for_slot(&vm.scope, slot);
//     value.into_handle()
// }

#[no_mangle]
pub extern "C" fn wrenSetSlotBool(c_vm: *mut WrenVM, c_slot: c_int, value: bool) {
    let vm = from_c_vm!(c_vm);
    let slot = usize::try_from(c_slot).unwrap();
    vm.vm.set_slot_bool(&vm.scope, slot, value)
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
    let vm = from_c_vm!(c_vm);
    let slot = usize::try_from(c_slot).unwrap();
    vm.vm
        .set_slot_double(&vm.scope, usize::try_from(slot).unwrap(), value);
}

#[no_mangle]
pub extern "C" fn wrenSetSlotNewList(c_vm: *mut WrenVM, c_slot: c_int) {
    let vm = from_c_vm!(c_vm);
    let slot = usize::try_from(c_slot).unwrap();
    vm.vm.set_slot_new_list(&vm.scope, slot);
}

#[no_mangle]
pub extern "C" fn wrenSetSlotNewMap(c_vm: *mut WrenVM, c_slot: c_int) {
    let vm = from_c_vm!(c_vm);
    let slot = usize::try_from(c_slot).unwrap();
    vm.vm.set_slot_new_map(&vm.scope, slot);
}

#[no_mangle]
pub extern "C" fn wrenSetSlotNull(c_vm: *mut WrenVM, c_slot: c_int) {
    let vm = from_c_vm!(c_vm);
    let slot = usize::try_from(c_slot).unwrap();
    vm.vm.set_slot_null(&vm.scope, slot);
}

#[no_mangle]
pub extern "C" fn wrenSetSlotString(c_vm: *mut WrenVM, c_slot: c_int, c_text: *const c_char) {
    let vm = from_c_vm!(c_vm);
    let slot = usize::try_from(c_slot).unwrap();
    let string = unsafe { CStr::from_ptr(c_text) }.to_str().unwrap().into();
    vm.vm.set_slot_string(&vm.scope, slot, string);
}

// #[no_mangle]
// pub extern "C" fn wrenSetSlotHandle(c_vm: *mut WrenVM, c_slot: c_int, c_handle: *mut WrenHandle) {
//     let vm = from_c_vm!(c_vm);
//     let value = unsafe { std::mem::transmute::<*mut WrenHandle, &mut Value>(c_handle) };
//     let slot = usize::try_from(c_slot).unwrap();
//     vm.vm.set_value_for_slot(slot, value.clone());
// }

#[no_mangle]
extern "C" fn wrenGetListCount(c_vm: *mut WrenVM, c_slot: c_int) -> c_int {
    let vm = from_c_vm!(c_vm);
    let slot = usize::try_from(c_slot).unwrap();
    let count = vm.vm.get_list_count(&vm.scope, slot);
    c_int::try_from(count).unwrap()
}

#[no_mangle]
pub extern "C" fn wrenGetListElement(
    c_vm: *mut WrenVM,
    c_list_slot: c_int,
    c_index: c_int,
    c_element_slot: c_int,
) {
    let vm = from_c_vm!(c_vm);
    let list_slot = usize::try_from(c_list_slot).unwrap();
    let index = usize::try_from(c_index).unwrap();
    let element_slot = usize::try_from(c_element_slot).unwrap();
    vm.vm
        .get_list_element(&vm.scope, list_slot, index, element_slot)
}

#[no_mangle]
pub extern "C" fn wrenSetListElement(
    c_vm: *mut WrenVM,
    c_list_slot: c_int,
    c_index: c_int,
    c_element_slot: c_int,
) {
    let vm = from_c_vm!(c_vm);
    let list_slot = usize::try_from(c_list_slot).unwrap();
    let index = usize::try_from(c_index).unwrap();
    let element_slot = usize::try_from(c_element_slot).unwrap();
    vm.vm
        .set_list_element(&vm.scope, list_slot, index, element_slot)
}

#[no_mangle]
pub extern "C" fn wrenInsertInList(
    c_vm: *mut WrenVM,
    c_list_slot: c_int,
    c_index: c_int,
    c_element_slot: c_int,
) {
    let vm = from_c_vm!(c_vm);
    let list_slot = usize::try_from(c_list_slot).unwrap();
    let index = usize::try_from(c_index).unwrap();
    let element_slot = usize::try_from(c_element_slot).unwrap();
    vm.vm
        .insert_in_list(&vm.scope, list_slot, index, element_slot)
}

#[no_mangle]
pub extern "C" fn wrenGetMapCount(c_vm: *mut WrenVM, c_slot: c_int) -> c_int {
    let vm = from_c_vm!(c_vm);
    let slot = usize::try_from(c_slot).unwrap();
    let value = vm.vm.value_for_slot(&vm.scope, slot);
    let map: LocalHandle<ObjMap> = value.try_downcast().unwrap();
    let count = map.borrow().len();
    c_int::try_from(count).unwrap()
}

#[no_mangle]
pub extern "C" fn wrenGetMapContainsKey(
    c_vm: *mut WrenVM,
    c_map_slot: c_int,
    c_key_slot: c_int,
) -> bool {
    let vm = from_c_vm!(c_vm);
    let map_slot = usize::try_from(c_map_slot).unwrap();
    let key_slot = usize::try_from(c_key_slot).unwrap();
    vm.vm.map_contains_key(&vm.scope, map_slot, key_slot)
}

#[no_mangle]
pub extern "C" fn wrenGetMapValue(
    c_vm: *mut WrenVM,
    c_map_slot: c_int,
    c_key_slot: c_int,
    c_value_slot: c_int,
) {
    let vm = from_c_vm!(c_vm);
    let map_slot = usize::try_from(c_map_slot).unwrap();
    let key_slot = usize::try_from(c_key_slot).unwrap();
    let value_slot = usize::try_from(c_value_slot).unwrap();
    vm.vm
        .get_map_value(&vm.scope, map_slot, key_slot, value_slot)
}

#[no_mangle]
pub extern "C" fn wrenSetMapValue(
    c_vm: *mut WrenVM,
    c_map_slot: c_int,
    c_key_slot: c_int,
    c_value_slot: c_int,
) {
    let vm = from_c_vm!(c_vm);
    let map_slot = usize::try_from(c_map_slot).unwrap();
    let key_slot = usize::try_from(c_key_slot).unwrap();
    let value_slot = usize::try_from(c_value_slot).unwrap();
    vm.vm
        .set_map_value(&vm.scope, map_slot, key_slot, value_slot)
}

#[no_mangle]
pub extern "C" fn wrenRemoveMapValue(
    c_vm: *mut WrenVM,
    c_map_slot: c_int,
    c_key_slot: c_int,
    c_removed_value_slot: c_int,
) {
    let vm = from_c_vm!(c_vm);
    let map_slot = usize::try_from(c_map_slot).unwrap();
    let key_slot = usize::try_from(c_key_slot).unwrap();
    let removed_value_slot = usize::try_from(c_removed_value_slot).unwrap();
    vm.vm
        .remove_map_value(&vm.scope, map_slot, key_slot, removed_value_slot)
}

#[no_mangle]
pub extern "C" fn wrenGetVariable(
    c_vm: *mut WrenVM,
    c_module: *const c_char,
    c_name: *const c_char,
    c_slot: c_int,
) {
    let vm = from_c_vm!(c_vm);
    let module_name = unsafe { CStr::from_ptr(c_module) }.to_str().unwrap();
    let variable_name = unsafe { CStr::from_ptr(c_name) }.to_str().unwrap();
    let slot = usize::try_from(c_slot).unwrap();
    vm.vm
        .get_variable(&vm.scope, module_name, variable_name, slot)
}

#[no_mangle]
pub extern "C" fn wrenHasVariable(
    c_vm: *mut WrenVM,
    c_module: *const c_char,
    c_name: *const c_char,
) -> bool {
    let vm = from_c_vm!(c_vm);
    let module_name = unsafe { CStr::from_ptr(c_module) }.to_str().unwrap();
    let variable_name = unsafe { CStr::from_ptr(c_name) }.to_str().unwrap();
    vm.scope
        .as_ref(&vm.vm.globals)
        .has_variable(module_name, variable_name)
}

#[no_mangle]
pub extern "C" fn wrenHasModule(c_vm: *mut WrenVM, c_module: *const c_char) -> bool {
    let vm = from_c_vm!(c_vm);
    let module = unsafe { CStr::from_ptr(c_module) }.to_str().unwrap();
    vm.scope.as_ref(&vm.vm.globals).has_module(module)
}

#[no_mangle]
pub extern "C" fn wrenAbortFiber(c_vm: *mut WrenVM, c_slot: c_int) {
    let vm = from_c_vm!(c_vm);
    let slot = usize::try_from(c_slot).unwrap();
    vm.vm.abort_fiber(&vm.scope, slot)
}

#[no_mangle]
pub extern "C" fn wrenGetUserData(c_vm: *mut WrenVM) -> *mut c_void {
    let vm = from_c_vm!(c_vm);
    vm.vm.c_config.user_data
}

#[no_mangle]
pub extern "C" fn wrenSetUserData(c_vm: *mut WrenVM, c_user_data: *mut c_void) {
    let vm = from_c_vm!(c_vm);
    vm.vm.c_config.user_data = c_user_data;
}
