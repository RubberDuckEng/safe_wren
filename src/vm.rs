// analog to wren_vm.c from wren_c.

include!(concat!(env!("OUT_DIR"), "/wren_core_source.rs"));

use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::rc::Rc;
use std::{str, usize};

use crate::compiler::{is_local_name, FnDebug, InputManager, Ops, Variable};
use crate::core::{init_base_classes, init_fn_and_fiber, register_core_primitives};
use crate::ffi::c_api::WrenConfiguration;
use crate::opt::random_bindings::{
    random_bind_foreign_class, random_bind_foreign_method, random_source,
};
use crate::value::*;
use crate::wren::{
    Configuration, DebugLevel, ForeignMethodFn, InterpretResult, LoadModuleResult, Slot,
};

type Result<T, E = VMError> = std::result::Result<T, E>;

// FIXME: Are Symbols the same as Local and Upvalue indicies?
// Do they have the same limits?
pub(crate) type Symbol = usize;

pub(crate) const CORE_MODULE_NAME: &str = "#core";

// The maximum number of module-level variables that may be defined at one time.
const MAX_MODULE_VARS: usize = 65536;

// The maximum number of arguments that can be passed to a method. Note that
// this limit may be hardcoded in other places in the VM.
pub(crate) const MAX_PARAMETERS: usize = 16;

// The maximum number of fields a class can have, including inherited fields.
// This is explicit in the bytecode since `CODE_CLASS` and `CODE_SUBCLASS` take
// a single byte for the number of fields. Note that it's 255 and not 256
// because creating a class takes the *number* of fields, not the *highest
// field index*.
pub(crate) const MAX_FIELDS: usize = 255;

// Internal VM Error, wrapped in RuntimeError for API.
#[derive(Debug)]
pub(crate) enum VMError {
    Error(String),
    FiberAbort(Value),
}

impl VMError {
    // Rename to error_str?
    pub(crate) fn from_str(msg: &str) -> VMError {
        VMError::Error(msg.into())
    }

    // Rename to error_string?
    pub(crate) fn from_string(msg: String) -> VMError {
        VMError::Error(msg)
    }

    fn as_try_return_value(&self) -> Value {
        match self {
            VMError::Error(string) => Value::from_str(string),
            VMError::FiberAbort(value) => value.clone(),
        }
    }
}

#[derive(Debug)]
pub(crate) struct Module {
    pub name: String,

    // Should this just be a map?  wren_utils.h suggests so?
    variables: Vec<Value>,
    variable_names: Vec<String>,
}

pub(crate) enum ModuleLimitError {
    TooManyVariables,
}

impl Module {
    pub(crate) fn lookup_symbol(&self, name: &str) -> Option<u16> {
        self.variable_names
            .iter()
            .position(|e| e.eq(name))
            .map(|s| s as u16)
    }

    pub(crate) fn variable_count(&self) -> usize {
        self.variables.len()
    }

    pub(crate) fn check_for_undefined_variables<F, E>(
        &self,
        since_index: usize,
        f: F,
    ) -> Result<(), E>
    where
        F: Fn(&str, usize) -> Result<(), E>,
    {
        for i in since_index..self.variables.len() {
            let value = &self.variables[i];
            let name = &self.variable_names[i];
            if let Some(line_number) = value.try_into_num() {
                f(name, line_number as usize)?;
            }
        }
        Ok(())
    }

    pub(crate) fn replace_implicit_definition(&mut self, symbol: u16, value: Value) {
        let index = symbol as usize;
        assert!(self.variables[index].is_num());
        self.variables[index] = value;
    }

    pub(crate) fn add_variable(
        &mut self,
        name: &str,
        value: Value,
    ) -> Result<u16, ModuleLimitError> {
        if self.variables.len() == MAX_MODULE_VARS {
            Err(ModuleLimitError::TooManyVariables)
        } else {
            self.variable_names.push(name.into());
            self.variables.push(value);
            Ok((self.variable_names.len() - 1) as u16)
        }
    }

    pub(crate) fn variable_by_name(&self, name: &str) -> Option<Value> {
        self.lookup_symbol(name)
            .map(|index| self.variables[index as usize].clone())
    }

    pub(crate) fn expect_class(&self, name: &str) -> Handle<ObjClass> {
        let symbol = self
            .lookup_symbol(name)
            .unwrap_or_else(|| panic!("failed to load {}", name));
        self.variables[symbol as usize]
            .try_into_class()
            .unwrap_or_else(|| panic!("failed to load {}", name))
    }
}

#[derive(Debug)]
pub struct FrameInfo {
    pub module: String,
    pub line: usize,
    pub fn_name: String,
}

#[derive(Debug)]
pub struct StackTrace {
    pub frames: Vec<FrameInfo>,
}

#[derive(Debug)]
pub struct RuntimeError {
    pub msg: String,
    pub stack_trace: StackTrace,
}

impl RuntimeError {
    fn from_error_value(value: Value, stack_trace: StackTrace) -> RuntimeError {
        let maybe_msg = value.try_into_string();
        RuntimeError {
            // [error object] matches wren_c wrenDebugPrintStackTrace
            msg: maybe_msg.unwrap_or_else(|| "[error object]".to_string()),
            stack_trace,
        }
    }
}

#[derive(Debug, Default)]
pub struct SymbolTable {
    names: Vec<String>,
}

impl SymbolTable {
    // This is called methodSymbol or wrenSymbolTableEnsure in wren_c
    pub fn ensure_symbol(&mut self, name: &str) -> Symbol {
        if let Some(index) = self.symbol_for_name(name) {
            return index;
        }

        // New symbol, so add it.
        self.names.push(name.into());
        self.names.len() - 1
    }

    fn symbol_for_name(&self, name: &str) -> Option<Symbol> {
        self.names.iter().position(|n| n.eq(name))
    }

    pub fn name_for_symbol(&self, symbol: Symbol) -> String {
        match self.names.get(symbol) {
            None => "<not found>".into(),
            Some(name) => name.clone(),
        }
    }

    pub fn count(&self) -> usize {
        self.names.len()
    }
}

pub struct CallFrame {
    // Program counter (offset into current code block)
    // wren_c calls this ip (instruction pointer) and makes
    // it an actual pointer.
    pub pc: usize,
    // The closure being executed.
    pub(crate) closure: Handle<ObjClosure>,
    pub stack_start: usize,
}

impl CallFrame {
    pub(crate) fn new(closure: Handle<ObjClosure>, stack_start: usize) -> CallFrame {
        CallFrame {
            pc: 0,
            closure,
            stack_start,
        }
    }
}

// FIXME: This should express all states instead of "other".
// Tracks how this fiber has been invoked, aside from the ways that can be
// detected from the state of other fields in the fiber.
pub enum FiberRunSource {
    // The fiber is being run from another fiber using a call to `try()`.
    Try,

    // The fiber was directly invoked by `runInterpreter()`. This means it's the
    // initial fiber used by a call to `wrenCall()` or `wrenInterpret()`.
    Root,
    // The fiber is invoked some other way. If [caller] is None then the fiber
    // was invoked using `call()`. If [numFrames] is zero, then the fiber has
    // finished running and is done. If [numFrames] is one and that frame's `ip`
    // points to the first byte of code, the fiber has not been started yet.
    Other,
}

struct Api {
    stack: Vec<Value>,
    error: Value,
}

impl Api {
    fn new(slots: usize) -> Api {
        Api {
            stack: vec![Value::Null; slots],
            error: Value::Null,
        }
    }

    fn with_stack(stack: Vec<Value>) -> Api {
        Api {
            stack,
            error: Value::Null,
        }
    }

    fn ensure(&mut self, slots: usize) {
        if slots >= self.stack.len() {
            self.stack.resize(slots + 1, Value::Null);
        }
    }

    fn into_return_value(self) -> Value {
        // Take without copy.
        self.stack.into_iter().next().unwrap()
    }
}

pub struct VM {
    // Current executing Fiber (should eventually be a list?)
    pub(crate) fiber: Option<Handle<ObjFiber>>,
    // Single global symbol table for all method names (matches wren_c)
    // Separate Struct to allow easier passing to register_primitive
    pub methods: SymbolTable,
    modules: HashMap<String, Handle<Module>>,

    // The Core module is created first (empty)
    pub(crate) core_module: Option<Handle<Module>>,
    // Then Class
    pub(crate) class_class: Option<Handle<ObjClass>>,
    // Then Fn and Fiber (which are used by wren_core.wren)
    pub(crate) fn_class: Option<Handle<ObjClass>>,
    pub(crate) fiber_class: Option<Handle<ObjClass>>,
    // Finally the rest of wren_core.wren
    pub(crate) core: Option<CoreClasses>,
    pub(crate) start_time: std::time::Instant,
    pub(crate) last_imported_module: Option<Handle<Module>>,
    pub(crate) config: Configuration,

    // Args for the foreign function being called.
    // wren_c calls this apiStack
    api: Option<Api>,
    // Hold onto the function pointers for the C API so we can
    // use rust function pointers in the normal config and just
    // look up the c ones here through the VM pointer.
    pub(crate) c_config: WrenConfiguration,
}

pub trait UserData {
    // Hack to allow downcasting?
    // https://stackoverflow.com/questions/33687447/how-to-get-a-reference-to-a-concrete-type-from-a-trait-object
    fn as_mut_any(&mut self) -> &mut dyn std::any::Any;
}

pub enum SlotType {
    Bool,
    Num,
    Foreign,
    List,
    Map,
    Null,
    String,
    Unknown,
}

// Rust implementations backing the wren_c API.  Eventually these should
// shift to be the *rust* API and follow *rust* patterns.  All the C-isms
// should move into c_api.rs.
impl VM {
    pub fn ensure_slots(&mut self, slots: usize) {
        match &mut self.api {
            None => self.api = Some(Api::new(slots)),
            Some(api) => api.ensure(slots),
        }
    }

    pub fn set_slot_string(&mut self, slot: Slot, string: &str) {
        assert!(self.api.is_some());
        if let Some(api) = &mut self.api {
            api.stack[slot] = Value::from_str(string);
        }
    }

    pub fn abort_fiber(&mut self, slot: Slot) {
        assert!(self.api.is_some());
        if let Some(api) = &mut self.api {
            api.error = api.stack[slot].clone();
        }
    }

    pub fn has_module(&self, module_name: &str) -> bool {
        self.modules.contains_key(module_name)
    }

    pub fn has_variable(&self, module_name: &str, variable_name: &str) -> bool {
        if let Some(module) = self.modules.get(module_name) {
            // What if the module is mut_borrowed for execution?
            module.borrow().lookup_symbol(variable_name).is_some()
        } else {
            // wren_c asserts in the case of the module not being defined.
            false
        }
    }

    pub fn get_variable(&mut self, module_name: &str, variable_name: &str, slot: Slot) {
        let value = if let Some(module) = self.modules.get(module_name) {
            // What if the module is mut_borrowed for execution?
            module
                .borrow()
                .variable_by_name(variable_name)
                .unwrap()
                .clone()
        } else {
            // wren_c asserts in the case of the module not being defined.
            unreachable!("Module not defined.");
        };
        self.set_value_for_slot(slot, value);
    }

    pub fn set_slot_new_foreign(
        &mut self,
        slot: Slot,
        class_slot: Slot,
        user_data: Box<dyn UserData>,
    ) {
        assert!(self.api.is_some());
        if let Some(api) = &mut self.api {
            let class = api.stack[class_slot]
                .try_into_class()
                .expect("Slot must hold a class.");
            assert!(matches!(class.borrow().source, ClassSource::Foreign));
            api.stack[slot] = Value::Foreign(new_handle(ObjForeign::new(class, user_data)))
        }
    }

    pub fn slot_count(&self) -> usize {
        match &self.api {
            None => 0,
            Some(api) => api.stack.len(),
        }
    }

    pub fn set_slot_null(&mut self, slot: Slot) {
        assert!(self.api.is_some());
        if let Some(api) = &mut self.api {
            api.stack[slot] = Value::Null;
        }
    }

    pub fn get_slot_double(&mut self, slot: Slot) -> f64 {
        assert!(self.api.is_some());
        self.api.as_ref().unwrap().stack[slot]
            .try_into_num()
            .expect("slot is not a num")
    }

    pub fn set_slot_double(&mut self, slot: Slot, num: f64) {
        assert!(self.api.is_some());
        if let Some(api) = &mut self.api {
            api.stack[slot] = Value::Num(num)
        }
    }

    pub fn set_slot_new_list(&mut self, slot: Slot) {
        assert!(self.api.is_some());
        let value = Value::List(self.new_list(vec![]));
        if let Some(api) = &mut self.api {
            api.stack[slot] = value;
        }
    }

    pub fn get_list_count(&mut self, slot: Slot) -> usize {
        assert!(self.api.is_some());
        let list = self.value_for_slot(slot).try_into_list().unwrap();
        let count = list.borrow().len();
        count
    }

    pub fn get_list_element(&mut self, list_slot: Slot, index: usize, element_slot: Slot) {
        assert!(self.api.is_some());
        let list_ref = self.value_for_slot(list_slot).try_into_list().unwrap();
        let list = list_ref.borrow();
        self.set_value_for_slot(element_slot, list.elements[index].clone())
    }

    pub fn set_list_element(&mut self, list_slot: Slot, index: usize, element_slot: Slot) {
        assert!(self.api.is_some());
        let list = self.value_for_slot(list_slot).try_into_list().unwrap();
        let element = self.value_for_slot(element_slot);
        list.borrow_mut().elements[index] = element.clone();
    }

    pub fn insert_in_list(&mut self, list_slot: Slot, index: usize, element_slot: Slot) {
        assert!(self.api.is_some());
        let list = self.value_for_slot(list_slot).try_into_list().unwrap();
        let element = self.value_for_slot(element_slot);
        list.borrow_mut().elements.insert(index, element.clone());
    }

    pub fn set_slot_new_map(&mut self, slot: Slot) {
        assert!(self.api.is_some());
        let value = Value::Map(self.new_map());
        if let Some(api) = &mut self.api {
            api.stack[slot] = value;
        }
    }

    pub fn map_contains_key(&self, map_slot: Slot, key_slot: Slot) -> bool {
        assert!(self.api.is_some());
        let map = self
            .value_for_slot(map_slot)
            .try_into_map()
            .expect("slot is not a map");
        let key = self.value_for_slot(key_slot);
        let contains = map.borrow().contains_key(key);
        contains
    }

    pub fn get_map_value(&mut self, map_slot: Slot, key_slot: Slot, value_slot: Slot) {
        assert!(self.api.is_some());
        let map = self
            .value_for_slot(map_slot)
            .try_into_map()
            .expect("slot is not a map");
        let key = self.value_for_slot(key_slot);
        let value = map.borrow().data[key].clone();
        self.set_value_for_slot(value_slot, value)
    }

    pub fn set_map_value(&self, map_slot: Slot, key_slot: Slot, value_slot: Slot) {
        assert!(self.api.is_some());
        let map = self
            .value_for_slot(map_slot)
            .try_into_map()
            .expect("slot is not a map");
        let key = self.value_for_slot(key_slot).clone();
        let value = self.value_for_slot(value_slot).clone();
        map.borrow_mut().data.insert(key, value);
    }

    pub fn remove_map_value(&mut self, map_slot: Slot, key_slot: Slot, removed_value_slot: Slot) {
        assert!(self.api.is_some());
        let map = self
            .value_for_slot(map_slot)
            .try_into_map()
            .expect("slot is not a map");
        let key = self.value_for_slot(key_slot);
        let value = map.borrow_mut().data.remove(key).unwrap_or(Value::Null);
        self.set_value_for_slot(removed_value_slot, value);
    }

    pub fn get_slot_bool(&mut self, slot: Slot) -> bool {
        assert!(self.api.is_some());
        self.api.as_ref().unwrap().stack[slot]
            .try_into_bool()
            .expect("slot is not a bool")
    }

    pub fn set_slot_bool(&mut self, slot: Slot, value: bool) {
        assert!(self.api.is_some());
        if let Some(api) = &mut self.api {
            api.stack[slot] = Value::Boolean(value)
        }
    }

    // This isn't quite right for the rust API, but is used by the C API.
    pub(crate) fn call_handle_for_signature(&mut self, signature: &str) -> Value {
        let symbol = self.methods.ensure_symbol(signature);
        // Create a little stub function that assumes the arguments are on the
        // stack and calls the method.
        let fn_obj = ObjFn::stub_call(self, signature, symbol);
        let closure = ObjClosure::new(self, new_handle(fn_obj));
        Value::Closure(new_handle(closure))
    }

    // This is just a (non-functional) stub for the C API as well.
    pub(crate) fn call(&mut self, _method: &Value) -> InterpretResult {
        self.set_value_for_slot(0, Value::Null);
        InterpretResult::Success
    }

    // Allows c_api to write addition APIs which the rust public API
    // may not also share.
    pub(crate) fn value_for_slot(&self, slot: Slot) -> &Value {
        &self.api.as_ref().unwrap().stack[slot]
    }

    pub(crate) fn set_value_for_slot(&mut self, slot: Slot, value: Value) {
        assert!(self.api.is_some());
        if let Some(api) = &mut self.api {
            api.stack[slot] = value;
        }
    }

    // Maybe return Option<SlotType> and None on failure instead of panick?
    pub fn type_for_slot(&self, slot: Slot) -> SlotType {
        match self.value_for_slot(slot) {
            Value::Boolean(_) => SlotType::Bool,
            Value::Num(_) => SlotType::Num,
            Value::Foreign(_) => SlotType::Foreign,
            Value::List(_) => SlotType::List,
            Value::Map(_) => SlotType::Map,
            Value::Null => SlotType::Null,
            Value::String(_) => SlotType::String,
            _ => SlotType::Unknown,
        }
    }
}

pub trait Clear {
    fn clear(&mut self);
}

impl Clear for Module {
    fn clear(&mut self) {
        self.variables.clear();
        self.variable_names.clear();
    }
}

pub fn clear_class(maybe_class: Option<Handle<ObjClass>>) {
    if let Some(class) = maybe_class {
        class.take().clear();
    }
}

fn clear_maybe_module(maybe_module: Option<Handle<Module>>) {
    if let Some(module) = maybe_module {
        module.borrow_mut().clear();
    }
}

impl Drop for VM {
    fn drop(&mut self) {
        // Tear down all fibers, including all stacks.
        // Eventually code will be able to hold onto fibers.
        if let Some(fiber) = &self.fiber {
            fiber.borrow_mut().clear();
        }

        // Modules keep references to functions
        // functions keep references to modules.
        clear_maybe_module(self.core_module.take());
        clear_maybe_module(self.last_imported_module.take());
        for module in self.modules.values() {
            module.borrow_mut().clear();
        }

        // Classes hold onto Methods, which include Functions.
        clear_class(self.class_class.take());
        clear_class(self.fn_class.take());
        clear_class(self.fiber_class.take());

        if let Some(core) = self.core.take() {
            core.num.borrow_mut().clear();
            core.bool_class.borrow_mut().clear();
            core.null.borrow_mut().clear();
            core.string.borrow_mut().clear();
            core.range.borrow_mut().clear();
            core.list.borrow_mut().clear();
            core.map.borrow_mut().clear();
        }
    }
}

impl core::fmt::Debug for VM {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "VM {{ ")?;
        if let Some(fiber) = &self.fiber {
            write!(f, "stack: {:?}, ", fiber.borrow())?;
        }
        write!(f, "methods: (len {}) ", self.methods.names.len())?;
        write!(f, "}}")
    }
}

impl VM {
    pub(crate) fn new_list(&self, contents: Vec<Value>) -> Handle<ObjList> {
        new_handle(ObjList {
            class_obj: self.core.as_ref().unwrap().list.clone(),
            elements: contents,
        })
    }

    pub(crate) fn new_map(&self) -> Handle<ObjMap> {
        new_handle(ObjMap {
            class_obj: self.core.as_ref().unwrap().map.clone(),
            data: HashMap::new(),
        })
    }

    pub(crate) fn new_range(&self, from: f64, to: f64, is_inclusive: bool) -> Handle<ObjRange> {
        new_handle(ObjRange {
            class_obj: self.core.as_ref().unwrap().range.clone(),
            from,
            to,
            is_inclusive,
        })
    }

    pub(crate) fn new_fiber(&self, closure: Handle<ObjClosure>) -> Handle<ObjFiber> {
        new_handle(ObjFiber::new(self, closure, FiberRunSource::Other))
    }

    pub(crate) fn new_class(
        &self,
        superclass: &Handle<ObjClass>,
        source: ClassSource,
        name: String,
    ) -> Result<Handle<ObjClass>> {
        new_class_with_class_class(
            superclass,
            source,
            name,
            &self.class_class.as_ref().unwrap(),
        )
    }
}

fn new_single_class(source: ClassSource, name: String) -> Handle<ObjClass> {
    // the wren_c version does a lot more?  Unclear if this should.
    new_handle(ObjClass {
        name,
        methods: Vec::new(),
        class: None,
        superclass: None,
        source,
    })
}

// This was made in hopes of sharing code with base_class but it turns
// out this is an interpret time function (only creates classes)
// and does not do any of the declaration work base_class does.
// Keeping it for now in case it's useful later.
fn new_class_with_class_class(
    superclass: &Handle<ObjClass>,
    source: ClassSource,
    name_string: String,
    class_class: &Handle<ObjClass>,
) -> Result<Handle<ObjClass>> {
    // Create the metaclass.

    let metaclass_name_string = format!("{} metaclass", name_string);
    // let metaclass_name = Value::from_string(metaclass_name_string);

    let metaclass = new_single_class(ClassSource::Internal, metaclass_name_string);
    metaclass.borrow_mut().class = Some(class_class.clone());

    // Metaclasses always inherit Class and do not parallel the non-metaclass
    // hierarchy.
    metaclass.borrow_mut().bind_superclass(class_class);

    let class = new_single_class(source, name_string);
    class.borrow_mut().class = Some(metaclass);
    class.borrow_mut().bind_superclass(superclass);

    Ok(class)
}

fn validate_superclass(
    name: &str,
    superclass_value: Value,
    source: &ClassSource,
) -> Result<Handle<ObjClass>> {
    // Make sure the superclass is a class.
    let superclass = superclass_value.try_into_class().ok_or_else(|| {
        VMError::from_string(format!(
            "Class '{}' cannot inherit from a non-class object.",
            name
        ))
    })?;

    // In wren_c, this is required since wren_c does blind-casts
    // of "this" in primitives.  safe_wren also does unwrap() and would
    // (safely) panic if "this" were a ObjInstance subclass.
    // FIXME: Merge with match below by checking ClassSource::Internal?
    match &superclass.borrow().name[..] {
        "Class" | "Fiber" | "Fn" | "List" | "Map" | "Range" | "String" | "Bool" | "Null"
        | "Num" => {
            return Err(VMError::from_string(format!(
                "Class '{}' cannot inherit from built-in class '{}'.",
                name,
                superclass.borrow().name
            )));
        }
        _ => {}
    }
    match (source, &superclass.borrow().source) {
        (_, ClassSource::Foreign) => {
            return Err(VMError::from_string(format!(
                "Class '{}' cannot inherit from foreign class '{}'.",
                name,
                superclass.borrow().name
            )));
        }
        (ClassSource::Foreign, ClassSource::Source(num_fields)) => {
            if *num_fields > 0 {
                return Err(VMError::from_string(format!(
                    "Foreign class '{}' may not inherit from a class with fields.",
                    name,
                )));
            }
        }
        (ClassSource::Source(fields), ClassSource::Source(super_fields)) => {
            if *fields + *super_fields > MAX_FIELDS {
                return Err(VMError::from_string(format!(
                    "Class '{}' may not have more than 255 fields, including inherited ones.",
                    name
                )));
            }
        }
        _ => {}
    }

    Ok(superclass)
}

fn bind_foreign_class(vm: &mut VM, class: &mut ObjClass, module: &Module) {
    // Add the symbol even if there is no allocator so we can ensure that the
    // symbol itself is always in the symbol table.
    let allocate_symbol = vm.methods.ensure_symbol("<allocate>");
    // Add the symbol even if there is no finalizer so we can ensure that the
    // symbol itself is always in the symbol table.
    // let finalize_symbol = vm.methods.ensure_symbol("<finalize>");

    // Check the optional built-in module first so the host can override it.
    if let Some(bind_class) = vm.config.bind_foreign_class_fn {
        let methods = bind_class(vm, &module.name, &class.name);
        class.set_method(allocate_symbol, Method::ForeignFunction(methods.allocate));
        // if let Some(finalize) = methods.finalize {
        //     class.set_method(finalize_symbol, Method::ForeignFunction(finalize));
        // }
    } else {
        // wren_c does not, but we could error here, since we know we have
        // no <allocate> for the class.
        // assert!("Allocator required for foreign classes");
    }

    if module.name.eq("random") {
        let methods = random_bind_foreign_class(vm, &module.name, &class.name);
        class.set_method(allocate_symbol, Method::ForeignFunction(methods.allocate));
        // if let Some(finalize) = methods.finalize {
        //     class.set_method(finalize_symbol, Method::ForeignFunction(finalize));
        // }
    }
}

fn create_class(vm: &mut VM, fiber: &ObjFiber, source: ClassSource, module: &Module) -> Result<()> {
    // Pull the name and superclass off the stack.
    let superclass_value = fiber.pop()?;
    let name_value = fiber.pop()?;

    let name = name_value
        .try_into_string()
        .ok_or_else(|| VMError::from_str("Class name not string."))?;
    let superclass = validate_superclass(&name, superclass_value, &source)?;

    let class = vm.new_class(&superclass, source.clone(), name)?;
    if let ClassSource::Foreign = source {
        bind_foreign_class(vm, &mut class.borrow_mut(), module)
    }
    // After bind_foreign_class to avoid a clone, should not make a differnce.
    fiber.push(Value::Class(class));
    Ok(())
}

type Handle<T> = Rc<RefCell<T>>;

pub(crate) fn new_handle<T>(t: T) -> Handle<T> {
    Rc::new(RefCell::new(t))
}

#[derive(Debug)]
pub(crate) struct CoreClasses {
    pub(crate) num: Handle<ObjClass>,
    pub(crate) bool_class: Handle<ObjClass>,
    pub(crate) null: Handle<ObjClass>,
    pub(crate) string: Handle<ObjClass>,
    pub(crate) range: Handle<ObjClass>,
    pub(crate) list: Handle<ObjClass>,
    pub(crate) map: Handle<ObjClass>,
}

#[derive(Debug)]
pub enum DefinitionError {
    VariableAlreadyDefined,                   // -1 in wren_c
    TooManyVariables,                         // -2 in wren_c
    LocalUsedBeforeDefinition(String, usize), // -3 in wren_c
}

impl From<ModuleLimitError> for DefinitionError {
    fn from(err: ModuleLimitError) -> DefinitionError {
        match err {
            ModuleLimitError::TooManyVariables => DefinitionError::TooManyVariables,
        }
    }
}

impl Module {
    pub(crate) fn define_variable(
        &mut self,
        name: &str,
        value: Value,
    ) -> Result<u16, DefinitionError> {
        // See if the variable is already explicitly or implicitly declared.
        match self.lookup_symbol(name) {
            None => {
                // New variable!
                self.add_variable(name, value).map_err(From::from)
            }
            Some(symbol) => {
                let existing_value = &self.variables[symbol as usize];
                if let Some(line) = existing_value.try_into_num() {
                    if is_local_name(name) {
                        return Err(DefinitionError::LocalUsedBeforeDefinition(
                            name.into(),
                            line as usize,
                        ));
                    }
                    // An implicitly declared variable's value will always be a number.
                    // Now we have a real definition.
                    self.replace_implicit_definition(symbol, value);
                    Ok(symbol)
                } else {
                    Err(DefinitionError::VariableAlreadyDefined)
                }
            }
        }
    }
}

// NOTE: This is only designed for Object and Class and does not fully
// wire up a class!
pub(crate) fn define_class(module: &mut Module, name: &str) -> Handle<ObjClass> {
    let class = new_handle(ObjClass {
        name: name.into(),
        methods: Vec::new(),
        class: None,
        superclass: None,
        source: ClassSource::Internal,
    });

    module
        .define_variable(name, Value::Class(class.clone()))
        .expect("defined");
    class
}

impl VM {
    pub(crate) fn load_wren_core(&mut self, module_name: &str) {
        // wren_core_source() is generated by build.rs from wren_core.wren
        let source = wren_core_source();
        let input = InputManager::from_str(source);
        let closure = self
            .compile_in_module(module_name, input)
            .expect("compile wren_core");
        // debug_bytecode(vm, &closure.borrow(), module);
        self.run(closure).expect("run wren_core");
    }
}

enum FunctionNext {
    Call(Handle<ObjClosure>, usize),
    Return(Value),
    FiberAction(FiberAction),
    CaptureUpvalues(Handle<ObjClosure>, Vec<crate::compiler::Upvalue>),
    CloseUpvalues(StackOffset),
}

// FIXME: Unclear if this is needed now that we have a single unified stack!
#[derive(Copy, Clone, Debug)]
pub struct StackOffset {
    // CAUTION: It's very easy to create this with a wrong index.
    pub frame_index: usize,
    // index is assumed to be relative, to the stack_start
    // which is referenced in the frame refered to by the frame_index.
    pub index: usize,
}

impl Eq for StackOffset {}

impl PartialEq for StackOffset {
    fn eq(&self, rhs: &StackOffset) -> bool {
        self.frame_index == rhs.frame_index && self.index == rhs.index
    }
}

impl PartialOrd for StackOffset {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for StackOffset {
    fn cmp(&self, other: &Self) -> Ordering {
        let result = self.frame_index.cmp(&other.frame_index);
        if !matches!(result, Ordering::Equal) {
            result
        } else {
            self.index.cmp(&other.index)
        }
    }
}

// The dynamically allocated data structure for a variable that has been used
// by a closure. Whenever a function accesses a variable declared in an
// enclosing function, it will get to it through this.
//
// An upvalue can be either "closed" or "open". An open upvalue points directly
// to a [Value] that is still stored on the fiber's stack because the local
// variable is still in scope in the function where it's declared.
//
// When that local variable goes out of scope, the upvalue pointing to it will
// be closed. When that happens, the value gets copied off the stack into the
// upvalue itself. That way, it can have a longer lifetime than the stack
// variable.

#[derive(Debug)]
pub(crate) enum UpvalueStorage {
    Open(Handle<ObjFiber>, StackOffset),
    Closed(Value),
}

#[derive(Debug)]
pub(crate) struct Upvalue {
    pub storage: UpvalueStorage,
}

impl Upvalue {
    pub fn new(fiber: Handle<ObjFiber>, location: StackOffset) -> Handle<Upvalue> {
        new_handle(Upvalue {
            storage: UpvalueStorage::Open(fiber, location),
        })
    }

    pub fn is_open(&self) -> bool {
        matches!(&self.storage, UpvalueStorage::Open(_, _))
    }

    pub fn has_location(&self, location: &StackOffset) -> bool {
        matches!(&self.storage, UpvalueStorage::Open(_, l) if l.eq(location))
    }

    pub fn location(&self) -> Option<StackOffset> {
        match &self.storage {
            UpvalueStorage::Open(_, l) => Some(*l),
            UpvalueStorage::Closed(_) => None,
        }
    }

    fn load(&self) -> Value {
        match &self.storage {
            UpvalueStorage::Open(fiber, loc) => fiber.borrow().load(*loc),
            UpvalueStorage::Closed(value) => value.clone(),
        }
    }

    fn store(&mut self, new_value: Value) {
        match &mut self.storage {
            UpvalueStorage::Open(fiber, loc) => fiber.borrow().store(*loc, new_value),
            UpvalueStorage::Closed(value) => *value = new_value,
        }
    }
}

// Looks up a foreign method in [moduleName] on [className] with [signature].
//
// This will try the host's foreign method binder first. If that fails, it
// falls back to handling the built-in modules.
fn find_foreign_method(
    vm: &mut VM, // Only mut because random_bind_foreign_method expects mut.
    module_name: &str,
    class_name: &str,
    is_static: bool,
    signature: &str,
) -> Option<ForeignMethodFn> {
    if let Some(bind_foreign_method) = vm.config.bind_foreign_method_fn {
        if let Some(method_fn) =
            bind_foreign_method(vm, module_name, class_name, is_static, signature)
        {
            return Some(method_fn);
        }
    }

    if module_name.eq("random") {
        return Some(random_bind_foreign_method(
            vm, class_name, is_static, signature,
        ));
    }
    // FIXME: support opt_meta
    None
}

// Defines [methodValue] as a method on [classObj].
//
// Handles both foreign methods where [methodValue] is a string containing the
// method's signature and Wren methods where [methodValue] is a function.
//
// Aborts the current fiber if the method is a foreign method that could not be
// found.
fn bind_method(
    vm: &mut VM,
    is_static: bool,
    symbol: Symbol,
    module: &Module,
    class: &mut ObjClass,
    method_value: Value,
) -> Result<()> {
    let method = match method_value {
        Value::String(signature) => {
            let module_name = &module.name;
            let class_name = &class.name;
            let foreign_fn =
                find_foreign_method(vm, module_name, class_name, is_static, &signature)
                    .ok_or_else(|| {
                        VMError::from_string(format!(
                            "Could not find foreign method '{}' for class {} in module '{}'.",
                            signature, class_name, module_name
                        ))
                    })?;
            Method::ForeignFunction(foreign_fn)
        }
        Value::Closure(closure) => {
            // Patch up the bytecode now that we know the superclass.
            class.bind_method_code(&mut closure.borrow().fn_obj.borrow_mut());
            Method::Block(closure)
        }
        // Is this a compiler error?  Should this panic?
        _ => {
            return Err(VMError::from_str(
                "method value not a string (foreign) or closure (block",
            ))
        }
    };

    if is_static {
        class.class_obj().borrow_mut().set_method(symbol, method);
    } else {
        class.set_method(symbol, method);
    };
    Ok(())
}

// Let the host resolve an imported module name if it wants to.
fn resolve_module(vm: &mut VM, importer: &str, name: &str) -> String {
    if let Some(resolve_module) = vm.config.resolve_module_fn {
        resolve_module(vm, importer, name)
    } else {
        name.into()
    }
}

enum ImportResult {
    Existing(Handle<Module>),
    New(Handle<ObjClosure>),
}

fn try_load_module(vm: &mut VM, name: &str) -> Option<LoadModuleResult> {
    if let Some(load_module) = vm.config.load_module_fn {
        if let Some(result) = load_module(vm, name) {
            return Some(result);
        }
    }
    if name.eq("random") {
        return Some(LoadModuleResult {
            source: random_source(),
        });
    }
    None
}

fn import_module(vm: &mut VM, importer_name: &str, unresolved_name: &str) -> Result<ImportResult> {
    let name = resolve_module(vm, importer_name, unresolved_name);

    // If the module is already loaded, we don't need to do anything.
    if let Some(m) = vm.modules.get(&name) {
        return Ok(ImportResult::Existing(m.clone()));
    }

    let result = try_load_module(vm, &name)
        .ok_or_else(|| VMError::from_string(format!("Could not load module '{}'.", name)))?;

    let input = InputManager::from_string(result.source);
    let closure = vm
        .compile_in_module(&name, input)
        .map_err(|_| VMError::from_string(format!("Could not compile module '{}'.", name)))?;

    // Return the closure that executes the module.
    Ok(ImportResult::New(closure))
}

fn get_module_variable(_vm: &VM, module: &Module, variable_name: &str) -> Result<Value> {
    module.variable_by_name(variable_name).ok_or_else(|| {
        VMError::from_string(format!(
            "Could not find a variable named '{}' in module '{}'.",
            variable_name, module.name
        ))
    })
}

impl VM {
    pub fn new(config: Configuration) -> Self {
        let mut vm = Self {
            // Invalid import name, intentionally.
            methods: SymbolTable::default(),
            fiber: None,
            core_module: None,
            core: None,
            class_class: None,
            fn_class: None,
            fiber_class: None,
            modules: HashMap::new(),
            start_time: std::time::Instant::now(),
            last_imported_module: None,
            config,
            api: None,
            c_config: WrenConfiguration::default(),
        };

        // FIXME: This shouldn't be needed anymore.  We no longer
        // remove modules during parsing.
        // Modules are owned by the modules HashMap.
        // When compiled into, they are taken out of the HashMap.
        // Core is special.  We can't both compile core and have it
        // accessible for lookups at the same time, so we make this
        // stub, compile the real core, and then replace this.
        // FIXME: Does this mean that things compiled with the stub
        // will store their module-level variables in a different
        // module than the rest of core?
        let mut stub_core = Module {
            name: CORE_MODULE_NAME.into(),
            variables: Vec::new(),
            variable_names: Vec::new(),
        };
        init_base_classes(&mut vm, &mut stub_core);
        vm.class_class = Some(stub_core.expect_class("Class"));
        init_fn_and_fiber(&mut vm, &mut stub_core);
        vm.core_module = Some(new_handle(stub_core));
        let core_name = CORE_MODULE_NAME;
        vm.load_wren_core(core_name);
        vm.core_module = Some(vm.modules.remove(core_name).unwrap());
        register_core_primitives(&mut vm);
        vm
    }

    pub(crate) fn lookup_or_register_empty_module(&mut self, name: &str) -> Handle<Module> {
        if let Some(m) = self.modules.get(name) {
            return m.clone();
        }
        let module = self.new_module_with_name(name);
        self.register_module(module.clone());
        module
    }

    pub(crate) fn new_module_with_name(&self, name: &str) -> Handle<Module> {
        // We automatically import Core into all modules.
        // Implicitly import the core module.
        let core = self.core_module.as_ref().unwrap().borrow();
        new_handle(Module {
            name: name.into(),
            variable_names: core.variable_names.clone(),
            variables: core.variables.clone(),
        })
    }

    pub(crate) fn register_module(&mut self, module: Handle<Module>) {
        let name = module.borrow().name.clone();
        self.modules.insert(name, module);
    }

    // called wrenGetClass in wren_c.
    pub(crate) fn class_for_value(&self, value: &Value) -> Handle<ObjClass> {
        let core = self.core.as_ref().unwrap();
        match value {
            Value::Null => core.null.clone(),
            Value::Num(_) => core.num.clone(),
            Value::Boolean(_) => core.bool_class.clone(),
            Value::String(_) => core.string.clone(),
            Value::Class(o) => o.borrow().class_obj(),
            Value::List(o) => o.borrow().class_obj(),
            Value::Map(o) => o.borrow().class_obj(),
            Value::Range(o) => o.borrow().class_obj(),
            Value::Fiber(o) => o.borrow().class_obj(),
            Value::Fn(o) => o.borrow().class_obj(),
            Value::Closure(o) => o.borrow().class_obj(),
            Value::Instance(o) => o.borrow().class_obj(),
            Value::Foreign(o) => o.borrow().class_obj(),
        }
    }

    #[allow(dead_code)]
    fn print_methods(&self, class: &ObjClass) {
        println!("{:?} has:", class);
        for (symbol, method) in class.methods.iter().enumerate() {
            match method {
                None => (),
                Some(_) => {
                    println!(
                        "{} ({}) => {:?}",
                        self.methods.name_for_symbol(symbol),
                        symbol,
                        method
                    );
                }
            }
        }
    }

    fn method_not_found(&self, class: &ObjClass, symbol: Symbol) -> VMError {
        let name = self.methods.name_for_symbol(symbol);
        VMError::from_string(format!("{} does not implement '{}'.", class.name, name))
    }

    // Will this eventually have to walk across fibers?
    // Or can this just be a free function?
    fn stack_trace(&self, fiber: &ObjFiber) -> StackTrace {
        // Walk the fibers in reverse info.
        fn frame_info(frame: &CallFrame) -> FrameInfo {
            let closure = frame.closure.borrow();
            let fn_obj = closure.fn_obj.borrow();
            let module = fn_obj.module.borrow().name.clone();
            FrameInfo {
                module,
                // In an executing frame, the pc points to the *next*
                // instruction to execute, the error is from the
                // previous instruction.
                line: fn_obj.debug.line_for_pc(frame.pc - 1),
                fn_name: fn_obj.debug.name.clone(),
            }
        }

        StackTrace {
            frames: fiber
                .call_stack
                .borrow()
                .iter()
                .rev()
                .map(frame_info)
                .collect(),
        }
    }

    fn call_foreign(&mut self, foreign: &ForeignMethodFn, args: Vec<Value>) -> Result<Value> {
        assert!(self.api.is_none(), "Cannot already be in foreign call.");
        self.api = Some(Api::with_stack(args));
        foreign(self);
        let api = self.api.take().unwrap();
        if api.error.is_null() {
            Ok(api.into_return_value())
        } else {
            Err(VMError::FiberAbort(api.error))
        }
    }

    fn cascade_error(&mut self, error: Value) -> Result<(), RuntimeError> {
        let stack_trace_fiber = self.fiber.clone().unwrap();
        loop {
            let callee = self.fiber.clone().unwrap();
            // Set Fiber.error on the current fiber. Can't do this
            // deeper in the stack because can't borrow_mut there.
            callee.borrow_mut().error = error.clone();
            // If we have a caller, it's now the new fiber.
            let caller = match &self.fiber {
                Some(fiber) => fiber.borrow_mut().return_from_fiber_take_caller(),
                _ => None,
            };
            self.fiber = caller;

            match &self.fiber {
                // If the previously called fiber was a try, return
                // control and the error value.
                Some(fiber) => {
                    if callee.borrow().is_try() {
                        fiber.borrow_mut().push_return_value(error);
                        break;
                    }
                    // If this wasn't a try, continue walking up
                    // the fiber stack.
                }
                // If we've reached the root fiber, return.
                None => {
                    return Err(RuntimeError::from_error_value(
                        error,
                        self.stack_trace(&stack_trace_fiber.borrow()),
                    ))
                }
            }
        }
        Ok(())
    }

    pub(crate) fn run(&mut self, closure: Handle<ObjClosure>) -> Result<Value, RuntimeError> {
        self.fiber = Some(new_handle(ObjFiber::new(
            self,
            closure,
            FiberRunSource::Root,
        )));
        loop {
            let fiber = self.fiber.clone().unwrap();
            let result = self.run_in_fiber(&fiber);
            match result {
                Ok(FiberAction::Call(fiber, arg)) => {
                    fiber.borrow_mut().caller = self.fiber.take();
                    fiber.borrow_mut().push_call_arg_or_return_value(arg);
                    self.fiber = Some(fiber.clone());
                }
                Ok(FiberAction::Try(fiber, arg)) => {
                    fiber.borrow_mut().caller = self.fiber.take();
                    fiber.borrow_mut().push_call_arg_or_return_value(arg);
                    fiber.borrow_mut().run_source = FiberRunSource::Try;
                    self.fiber = Some(fiber.clone());
                }
                Ok(FiberAction::Suspend) => {
                    self.fiber = None;
                    // FIXME: This return value is wrong.
                    // The api should not return a value for Fiber.suspend.
                    return Ok(Value::Null);
                }
                Ok(FiberAction::Return(value)) => {
                    let caller = if let Some(fiber) = &self.fiber {
                        fiber.borrow_mut().return_from_fiber_take_caller()
                    } else {
                        // This should never be reached?
                        None
                    };
                    self.fiber = caller;
                    match &self.fiber {
                        Some(fiber) => fiber.borrow_mut().push_return_value(value),
                        None => return Ok(value),
                    }
                }
                Ok(FiberAction::Transfer(fiber, arg)) => {
                    fiber.borrow_mut().push_call_arg_or_return_value(arg);
                    self.fiber = Some(fiber.clone());
                }
                Ok(FiberAction::TransferError(fiber, error)) => {
                    self.fiber = Some(fiber);
                    self.cascade_error(error)?;
                }
                Err(error) => {
                    self.cascade_error(error.as_try_return_value())?;
                }
            }
        }
    }

    fn run_in_fiber(&mut self, fiber_ref: &Handle<ObjFiber>) -> Result<FiberAction, VMError> {
        loop {
            let fiber = &fiber_ref.borrow();
            // We pull the frame off of the call_stack so that in Debug mode
            // run_frame_in_fiber is still able to borrow the call_stack to
            // walk it for the active frame and frame_count.  We could pass that
            // debug info a different way I guess?
            let frame_index = fiber.call_stack.borrow().len() - 1;
            let mut frame = fiber.call_stack.borrow_mut().pop().unwrap();
            // This is all to avoid run_fiber needing to call itself
            // recursively, or the run_fiber main loop needing to pull
            // the frame on every instruction.  Maybe not worth it?
            let result = self.run_frame_in_fiber(&mut frame, fiber, frame_index);
            match result {
                Ok(FunctionNext::Call(closure, num_args)) => {
                    // Push the closure and the args on the stack?
                    let mut call_stack = fiber.call_stack.borrow_mut();
                    // call_stack does not contain "frame", restore it.
                    call_stack.push(frame);
                    // Now push our new frame!
                    call_stack.push(CallFrame {
                        pc: 0,
                        closure: closure,
                        stack_start: fiber.stack.borrow().len() - num_args,
                    });
                }
                Ok(FunctionNext::Return(value)) => {
                    // Because "return" can sometimes mean "yield", the wren
                    // compiler does not issue pops for locals upon return, nor
                    // does it close over upvalues, so we have to do that here.

                    // call_stack does not contain "frame", restore it.
                    fiber.call_stack.borrow_mut().push(frame);
                    // Fiber is already borrowed, so we can't borrow_mut here,
                    // thus OpenUpvalues is independently borrowed inside
                    let location = StackOffset {
                        frame_index,
                        index: 0,
                    };
                    fiber.close_upvalues_at_or_above(location);
                    // pop the frame again after upvalues are closed.
                    let frame = fiber.call_stack.borrow_mut().pop().unwrap();
                    fiber.stack.borrow_mut().truncate(frame.stack_start);

                    if fiber.call_stack.borrow().is_empty() {
                        return Ok(FiberAction::Return(value));
                    } else {
                        // Push the return value onto the calling stack.
                        fiber.push(value);
                    }
                }
                Ok(FunctionNext::FiberAction(action)) => {
                    // call_stack does not contain "frame", restore it.
                    fiber.call_stack.borrow_mut().push(frame);
                    return Ok(action);
                }
                // Not really a function action, but run_frame_in_fiber doesn't
                // have access to either the frame index or the fiber index.
                Ok(FunctionNext::CaptureUpvalues(closure, upvalues)) => {
                    // call_stack does not contain "frame", restore it.
                    fiber.call_stack.borrow_mut().push(frame);
                    // No need to call this if it's empty.
                    assert!(!upvalues.is_empty());
                    // wren_c stores upvalue information in 3 places, all of
                    // which should have the same length:
                    // FnObj::numUpvalues (just a count)
                    // Bytecode Upvalue isLocal, index (a stream thereof)
                    // ClosureObj::upvalues (actual value handles at runtime)
                    for i in 0..upvalues.len() {
                        let compiler_upvalue = &upvalues[i];

                        if compiler_upvalue.is_local_in_parent {
                            // Make an new upvalue to close over the parent's local variable.
                            let location = StackOffset {
                                frame_index: fiber.call_stack.borrow().len() - 1,
                                // FIXME: Confirm that compiler.upvalue.index
                                // is relative to frame.stack_start?
                                // If not, this is wrong.
                                index: compiler_upvalue.index,
                            };

                            let upvalue = find_or_create_upvalue(fiber_ref, location);
                            closure.borrow_mut().push_upvalue(upvalue);
                        } else {
                            // Use the same upvalue as the current call frame.
                            let stack = fiber.call_stack.borrow();
                            let top_frame = stack.last();
                            let frame = top_frame.as_ref().unwrap();
                            let upvalue = frame
                                .closure
                                .borrow()
                                .upvalue(compiler_upvalue.index)
                                .clone();
                            closure.borrow_mut().push_upvalue(upvalue);
                        }
                    }
                }
                Ok(FunctionNext::CloseUpvalues(location)) => {
                    // call_stack does not contain "frame", restore it.
                    fiber.call_stack.borrow_mut().push(frame);
                    // Fiber is already borrowed, so we can't borrow_mut here,
                    // thus OpenUpvalues is independently borrowed inside
                    fiber.close_upvalues_at_or_above(location);
                    // Now actualy do the pop.
                    fiber.pop()?;
                }
                Err(vm_error) => {
                    // Push the current frame back onto the fiber so we can
                    // see it in error reporting.
                    fiber.call_stack.borrow_mut().push(frame);
                    return Err(vm_error);
                }
            }
        }
    }

    fn should_dump_instructions(&self, fn_debug: &FnDebug) -> bool {
        match &self.config.debug_level {
            None => false,
            Some(level) => match level {
                DebugLevel::All => true,
                DebugLevel::NonCore => !fn_debug.from_core_module,
            },
        }
    }

    fn create_foreign(&mut self, class_handle: &Handle<ObjClass>) -> Result<Value> {
        // wren_c makes these all asserts, but I'm not sure why?
        fn vm_assert(condition: bool, msg: &str) -> Result<()> {
            if condition {
                Ok(())
            } else {
                Err(VMError::from_str(msg))
            }
        }

        let class = class_handle.borrow();
        vm_assert(class.is_foreign(), "Class must be a foreign class.")?;

        // wren_c TODO: Don't look up every time.
        let symbol = self
            .methods
            .symbol_for_name("<allocate>")
            .ok_or_else(|| VMError::from_str("Should have defined <allocate> symbol."))?;

        vm_assert(class.methods.len() > symbol, "Class should have allocator.")?;
        let method = class
            .lookup_method(symbol)
            .ok_or_else(|| VMError::from_str("Class should have allocator."))?;

        // Pass the constructor arguments to the allocator as well.
        match method {
            Method::ForeignFunction(foreign) => {
                let args = vec![Value::Class(class_handle.clone())];
                self.call_foreign(foreign, args)
            }
            _ => Err(VMError::from_str("Allocator should be foreign.")),
        }
    }

    // ~80% of benchmark time is spent under this function.
    fn call_method(
        &mut self,
        fiber: &ObjFiber,
        _frame: &mut CallFrame,
        num_args: usize,
        symbol: Symbol,
        class: &ObjClass,
    ) -> Result<FunctionNext, VMError> {
        // Get the Method record for this class for this symbol.
        let method = class
            .lookup_method(symbol)
            .ok_or_else(|| self.method_not_found(&class, symbol))?;

        let this_offset = fiber.stack.borrow().len() - num_args;

        // Even if we got a Method doesn't mean *this* class implements it.
        let result = match method {
            Method::ValuePrimitive(f) => {
                let value = f(self, &fiber.stack.borrow()[this_offset..])?;
                fiber.stack.borrow_mut().truncate(this_offset);
                FunctionNext::Return(value)
            }
            Method::FiberActionPrimitive(f) => {
                let action = f(self, &fiber.stack.borrow()[this_offset..])?;
                fiber.stack.borrow_mut().truncate(this_offset);
                FunctionNext::FiberAction(action)
            }
            Method::FunctionCall => {
                let args = &fiber.stack.borrow()[this_offset..];
                // Pushes a new stack frame.
                let closure = check_arity(&args[0], args.len())?;
                FunctionNext::Call(closure, num_args)
            }
            Method::ForeignFunction(foreign) => {
                // call_foriegn would copy the args anyway, so copy them here.
                let value =
                    self.call_foreign(foreign, fiber.stack.borrow_mut().split_off(this_offset))?;
                FunctionNext::Return(value)
            }
            Method::Block(closure) => FunctionNext::Call(closure.clone(), num_args),
        };
        Ok(result)
    }

    fn run_frame_in_fiber(
        &mut self,
        frame: &mut CallFrame,
        fiber: &ObjFiber,
        frame_index: usize, // for closures
    ) -> Result<FunctionNext> {
        // Copy out a ref, so we can later mut borrow the frame.
        // FIXME: Cloning every op *cannot* be correct!
        let closure_rc = frame.closure.clone();
        let closure = closure_rc.borrow();
        let fn_obj = closure.fn_obj.borrow();
        let dump_instructions = self.should_dump_instructions(&fn_obj.debug);
        loop {
            let op = &fn_obj.code[frame.pc];
            if dump_instructions {
                // Grab the "dynamic" scope (top module) since that
                // makes the most sense as the "owner" of the stack.
                let top_module_name = match &fiber.call_stack.borrow().first() {
                    None => module_name_and_line(&fn_obj, frame.pc),
                    Some(top_frame) => {
                        module_name_and_line(&top_frame.closure.borrow().fn_obj.borrow(), frame.pc)
                    }
                };
                let frame_depth = fiber.call_stack.borrow().len(); // Does not include current frame.
                fiber.dump_stack(frame, &top_module_name, frame_depth);
                dump_instruction(
                    frame.pc,
                    op,
                    &self.methods,
                    &frame.closure.borrow(),
                    Some(&fiber.stack.borrow()[frame.stack_start..]),
                    DumpMode::ShowSymbols,
                );
            }
            frame.pc += 1;
            match op {
                Ops::Constant(index) => {
                    fiber.push(fn_obj.lookup_constant(index).clone());
                }
                Ops::Boolean(value) => {
                    fiber.push(Value::Boolean(*value));
                }
                Ops::Null => {
                    fiber.push(Value::Null);
                }
                Ops::CallSuper(arity, symbol, constant) => {
                    // +1 for implicit arg for 'this'.
                    let num_args = arity.as_index() + 1;
                    // Compiler error if this is not a class.
                    let superclass = fn_obj.lookup_constant(constant).try_into_class().unwrap();
                    let action =
                        self.call_method(fiber, frame, num_args, *symbol, &superclass.borrow())?;
                    match action {
                        FunctionNext::Return(value) => {
                            // We got an immediate answer from a primitive or
                            // foreign call, we can handle it here and continue.
                            fiber.push(value);
                        }
                        _ => {
                            // Let the caller handle more complicated actions.
                            return Ok(action);
                        }
                    }
                }
                Ops::Call(arity, symbol) => {
                    // +1 for implicit arg for 'this'.
                    let num_args = arity.as_index() + 1;
                    let this_offset = fiber.stack.borrow().len() - num_args;
                    let this_class = self.class_for_value(&fiber.stack.borrow()[this_offset]);
                    let action =
                        self.call_method(fiber, frame, num_args, *symbol, &this_class.borrow())?;
                    match action {
                        FunctionNext::Return(value) => {
                            // We got an immediate answer from a primitive or
                            // foreign call, we can handle it here and continue.
                            fiber.push(value);
                        }
                        _ => {
                            // Let the caller handle more complicated actions.
                            return Ok(action);
                        }
                    }
                }
                // Called as part of constructors to fix "this" to be an
                // instance before continuing construction.
                Ops::Construct => {
                    let this = fiber.load_this(frame);
                    let class = this.try_into_class().expect("'this' should be a class.");
                    let instance = Value::Instance(new_handle(ObjInstance::new(class)));
                    fiber.store_this(frame, instance);
                }
                Ops::ForeignConstruct => {
                    let this = fiber.load_this(frame);
                    let class = this.try_into_class().expect("'this' should be a class.");
                    let instance = self.create_foreign(&class)?;
                    fiber.store_this(frame, instance);
                }
                Ops::Closure(constant, upvalues) => {
                    let fn_value = fn_obj.lookup_constant(constant).clone();
                    let fn_obj = fn_value
                        .try_into_fn()
                        .ok_or_else(|| VMError::from_str("constant was not closure"))?;
                    let closure = new_handle(ObjClosure::new(self, fn_obj));
                    // FIXME: Avoid this clone in the empty-upvalues case.
                    fiber.push(Value::Closure(closure.clone()));
                    if !upvalues.is_empty() {
                        return Ok(FunctionNext::CaptureUpvalues(closure, upvalues.to_vec()));
                    }
                    // Optimization: if no upvalues, continue as normal.
                }
                Ops::EndModule => {
                    self.last_imported_module = Some(fn_obj.module.clone());
                    fiber.push(Value::Null); // Is this the return value?
                }
                Ops::Return => {
                    // The top of our stack is returned and then the caller of
                    // this rust function will push the return onto the stack of
                    // the calling wren function CallFrame.
                    return Ok(FunctionNext::Return(fiber.pop()?));
                }
                Ops::CloseUpvalues => {
                    let absolute_index = fiber.stack.borrow().len() - 1;
                    let index = absolute_index - frame.stack_start;
                    let location = StackOffset { frame_index, index };
                    // We don't have a mut borrow for Fiber here, return out
                    // to a context which does to do the mutations.
                    return Ok(FunctionNext::CloseUpvalues(location));
                }
                Ops::Class(num_fields) => {
                    create_class(
                        self,
                        fiber,
                        ClassSource::Source(*num_fields),
                        &fn_obj.module.borrow(),
                    )?;
                }
                Ops::ForeignClass => {
                    create_class(self, fiber, ClassSource::Foreign, &fn_obj.module.borrow())?;
                }
                Ops::Load(variable) => {
                    let value = match *variable {
                        Variable::Module(index) => fn_obj.module.borrow().variables[index].clone(),
                        Variable::Upvalue(index) => closure.upvalue(index).borrow().load(),
                        Variable::Local(index) => fiber.load_local(frame, index),
                    };
                    fiber.push(value);
                }
                Ops::Store(variable) => {
                    let value = fiber.peek()?;
                    match *variable {
                        Variable::Module(index) => {
                            fn_obj.module.borrow_mut().variables[index] = value
                        }
                        Variable::Upvalue(index) => {
                            closure.upvalue(index).borrow_mut().store(value)
                        }
                        Variable::Local(index) => fiber.store_local(frame, index, value),
                    };
                }
                Ops::LoadField(symbol) => {
                    let receiver = fiber.pop()?;
                    let instance = receiver
                        .try_into_instance()
                        .ok_or_else(|| VMError::from_str("Receiver should be instance."))?;
                    if *symbol >= instance.borrow().fields.len() {
                        return Err(VMError::from_str("Out of bounds field."));
                    }
                    fiber.push(instance.borrow().fields[*symbol].clone());
                }
                Ops::StoreField(symbol) => {
                    let receiver = fiber.pop()?;
                    let instance = receiver
                        .try_into_instance()
                        .ok_or_else(|| VMError::from_str("Receiver should be instance."))?;
                    if *symbol >= instance.borrow().fields.len() {
                        return Err(VMError::from_str("Out of bounds field."));
                    }
                    instance.borrow_mut().fields[*symbol] = fiber.peek()?;
                }
                Ops::Pop => {
                    fiber.pop()?;
                }
                Ops::Method(is_static, symbol) => {
                    // wren_c peeks first then drops after bind, unclear why
                    let class = fiber.pop()?.try_into_class().unwrap();
                    let method = fiber.pop()?;
                    bind_method(
                        self,
                        *is_static,
                        *symbol,
                        &fn_obj.module.borrow(),
                        &mut class.borrow_mut(),
                        method,
                    )?;
                }
                Ops::End => {
                    panic!("Compile error: END instruction should never be reached.");
                }
                Ops::ImportModule(module_name) => {
                    // Make a slot on the stack for the module's fiber to place the return
                    // value. It will be popped after this fiber is resumed. Store the
                    // imported module's closure in the slot in case a GC happens when
                    // invoking the closure.
                    let result = import_module(self, &fn_obj.module.borrow().name, module_name)?;
                    // frame.push(value.clone());
                    // Check for fiber error?
                    // if wrenHasError(fiber) RUNTIME_ERROR();

                    // If we get a closure, call it to execute the module body.
                    match result {
                        ImportResult::New(closure) => {
                            // Imports are a bit like calling a function, except with zero args.
                            return Ok(FunctionNext::Call(closure, 0));
                        }
                        ImportResult::Existing(module) => {
                            // The module has already been loaded. Remember it so we can import
                            // variables from it if needed.
                            fiber.push(Value::from_str("dummy for module"));
                            self.last_imported_module = Some(module);
                        }
                    }
                }
                Ops::ImportVariable(variable_name) => {
                    let module = self
                        .last_imported_module
                        .as_ref()
                        .expect("Should have already imported module.");
                    let value = get_module_variable(self, &module.borrow(), variable_name)?;
                    fiber.push(value);
                }
                Ops::Loop(offset_backwards) => {
                    frame.pc -= *offset_backwards as usize;
                }
                Ops::Jump(offset_forward) => {
                    frame.pc += *offset_forward as usize;
                }
                Ops::JumpIfFalse(offset_forward) => {
                    let value = fiber.pop()?;
                    if value.is_falsey() {
                        frame.pc += *offset_forward as usize;
                    }
                }
                Ops::And(offset_forward) => {
                    // This differs from JumpIfFalse in whether it pops
                    let value = fiber.peek()?;
                    if value.is_falsey() {
                        frame.pc += *offset_forward as usize;
                    } else {
                        fiber.pop()?;
                    }
                }
                Ops::Or(offset_forward) => {
                    let value = fiber.peek()?;
                    if !value.is_falsey() {
                        frame.pc += *offset_forward as usize;
                    } else {
                        fiber.pop()?;
                    }
                }
                Ops::ClassPlaceholder => unimplemented!(),
                Ops::JumpIfFalsePlaceholder => unimplemented!(),
                Ops::JumpPlaceholder => unimplemented!(),
                Ops::OrPlaceholder => unimplemented!(),
                Ops::AndPlaceholder => unimplemented!(),
            }
        }
    }
}

fn check_arity(value: &Value, num_args: usize) -> Result<Handle<ObjClosure>> {
    let closure = value
        .try_into_closure()
        .expect("Receiver must be a closure.");

    // num_args includes implicit this, not counted in arity.
    let arity = closure.borrow().fn_obj.borrow().arity.as_index();
    if num_args > arity {
        Ok(closure)
    } else {
        Err(VMError::from_str("Function expects more arguments."))
        // I guess too many arguments is OK?
    }
}

enum DumpMode {
    ShowSymbols,
    HideSymbols,
}

fn module_name_and_line(fn_obj: &ObjFn, pc: usize) -> String {
    format!(
        "{}:{}",
        std::path::Path::new(&fn_obj.module.borrow().name)
            .file_name()
            .unwrap()
            .to_str()
            .unwrap(),
        fn_obj.debug.line_for_pc(pc)
    )
}

fn dump_instruction(
    pc: usize,
    op: &Ops,
    methods: &SymbolTable,
    closure: &ObjClosure,
    stack: Option<&[Value]>,
    mode: DumpMode,
) {
    let fn_obj = &closure.fn_obj.borrow();
    println!(
        "{}:{} {:02}: {}",
        module_name_and_line(fn_obj, pc),
        fn_obj.debug.name,
        pc,
        op_debug_string(op, methods, fn_obj, Some(closure), stack, mode)
    );
}

fn op_debug_string(
    op: &Ops,
    methods: &SymbolTable,
    fn_obj: &ObjFn,
    closure: Option<&ObjClosure>,
    stack: Option<&[Value]>,
    mode: DumpMode,
) -> String {
    // If stable_output do not print the symbol, otherwise every time we
    // change wren_core.wren all compile.txt files change.
    let unstable_num_arrow = |symbol: Symbol| match mode {
        DumpMode::HideSymbols => format!(""),
        DumpMode::ShowSymbols => format!("{} -> ", symbol),
    };
    let comma_unstable_num = |symbol: Symbol| match mode {
        DumpMode::HideSymbols => format!(""),
        DumpMode::ShowSymbols => format!(" {}", symbol),
    };

    match op {
        Ops::Constant(i) => format!("Constant({}: {:?})", i, fn_obj.lookup_constant(i)),
        Ops::Boolean(b) => format!("Boolean {}", b),
        Ops::Null => format!("{:?}", op),
        Ops::CallSuper(_arity, symbol, _super_constant) => {
            format!(
                "CallSuper({}{})",
                unstable_num_arrow(*symbol),
                methods.name_for_symbol(*symbol)
            )
        }
        Ops::Call(_arity, symbol) => {
            format!(
                "Call({}{})",
                unstable_num_arrow(*symbol),
                methods.name_for_symbol(*symbol)
            )
        }
        Ops::Load(v) => match *v {
            Variable::Local(index) => match stack {
                // Do not hide symbols for locals, they're stable-enough.
                Some(s) => format!("Load(Local, {} -> {:?})", index, s[index]),
                None => format!("Load(Local, {})", index),
            },
            Variable::Upvalue(index) => match closure {
                Some(c) => format!(
                    "Load(Upvalue, {} -> {:?})",
                    index,
                    c.upvalue(index).borrow()
                ),
                None => format!("Load(Upvalue, {})", index),
            },
            Variable::Module(index) => {
                format!(
                    "Load(Module, {}{:?})",
                    unstable_num_arrow(index),
                    fn_obj.module.borrow().variables[index]
                )
            }
        },
        Ops::Store(v) => match *v {
            Variable::Local(index) => format!("Store(Local, {})", index),
            Variable::Upvalue(index) => format!("Store(Upvalue, {})", index),
            Variable::Module(index) => format!("Store(Module{})", comma_unstable_num(index)),
        },
        Ops::LoadField(field) => format!("LoadField({})", field),
        Ops::StoreField(field) => format!("StoreField({})", field),
        Ops::JumpIfFalsePlaceholder => format!("{:?}", op),
        Ops::JumpIfFalse(_) => format!("{:?}", op),
        Ops::JumpPlaceholder => format!("{:?}", op),
        Ops::And(_) => format!("{:?}", op),
        Ops::AndPlaceholder => format!("{:?}", op),
        Ops::Or(_) => format!("{:?}", op),
        Ops::OrPlaceholder => format!("{:?}", op),
        Ops::ClassPlaceholder => format!("{:?}", op),
        Ops::Construct => format!("{:?}", op),
        Ops::ForeignConstruct => format!("{:?}", op),
        Ops::Method(is_static, symbol) => {
            let dispatch = if *is_static { "static" } else { "instance" };
            format!(
                "Method({}, {}{})",
                dispatch,
                unstable_num_arrow(*symbol),
                methods.name_for_symbol(*symbol)
            )
        }
        Ops::Closure(_, _) => format!("{:?}", op),
        Ops::Class(num_fields) => format!("Class({} fields)", num_fields),
        Ops::ForeignClass => format!("{:?}", op),
        Ops::Jump(_) => format!("{:?}", op),
        Ops::Loop(_) => format!("{:?}", op),
        Ops::Pop => format!("{:?}", op),
        Ops::Return => format!("{:?}", op),
        Ops::CloseUpvalues => format!("{:?}", op),
        Ops::EndModule => format!("{:?}", op),
        Ops::End => format!("{:?}", op),
        Ops::ImportModule(_) => format!("{:?}", op),
        Ops::ImportVariable(_) => format!("{:?}", op),
    }
}

pub(crate) fn debug_bytecode(vm: &VM, top_closure: &ObjClosure) {
    let mut fn_objs = vec![top_closure.fn_obj.clone()];

    loop {
        let fn_obj = fn_objs.remove(0);
        let func = &fn_obj.borrow();
        let fn_name = &func.debug.name;
        if !func.constants.is_empty() {
            println!("{} Constants:", fn_name);
            for (id, value) in func.constants.iter().enumerate() {
                println!("{:02}: {:?}", id, value);
            }
        }
        println!("{} Code:", fn_name);
        for (pc, op) in func.code.iter().enumerate() {
            // Don't share with dump_instruction for now.
            println!(
                "{:02} (ln {}): {}",
                pc,
                func.debug.line_for_pc(pc),
                op_debug_string(op, &vm.methods, func, None, None, DumpMode::HideSymbols)
            );
        }
        // Walk module-level constants looking for code?
        for const_value in &func.constants {
            if let Value::Fn(fn_obj) = const_value {
                fn_objs.push(fn_obj.clone());
            }
        }
        // Walk functions/closures looking for code?

        if fn_objs.is_empty() {
            break;
        }
    }
}

pub(crate) enum FiberAction {
    Call(Handle<ObjFiber>, Value),
    Transfer(Handle<ObjFiber>, Value),
    TransferError(Handle<ObjFiber>, Value),
    // AFAICT, Return and Yield are the same.
    Return(Value),
    Try(Handle<ObjFiber>, Value),
    Suspend,
}

// Unclear if this should take Vec or a slice?
type ValuePrimitive = fn(vm: &VM, args: &[Value]) -> Result<Value>;
type FiberActionPrimitive = fn(vm: &VM, args: &[Value]) -> Result<FiberAction>;

#[derive(Clone)]
pub(crate) enum Method {
    // A primitive method implemented in C in the VM. Unlike foreign methods,
    // this can directly manipulate the fiber's stack.
    ValuePrimitive(ValuePrimitive),
    FiberActionPrimitive(FiberActionPrimitive),

    // A primitive that handles .call on Fn.
    FunctionCall,

    // A externally-defined method.
    ForeignFunction(ForeignMethodFn),

    // A normal user-defined method.
    Block(Handle<ObjClosure>),
}

impl core::fmt::Debug for Method {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Method::ValuePrimitive(_) => write!(f, "ValuePrimitive"),
            Method::FiberActionPrimitive(_) => write!(f, "FiberActionPrimitive"),
            Method::FunctionCall => write!(f, "FunctionCall"),
            Method::Block(closure) => {
                write!(f, "Block {}", closure.borrow().fn_obj.borrow().debug.name)
            }
            Method::ForeignFunction(_) => write!(f, "ForeignFunction"),
        }
    }
}
#[derive(Clone, Debug)]
pub(crate) enum ClassSource {
    Internal,      // vm defined, no fields.
    Source(usize), // source defined, fields allowed
    Foreign,       // api defined, fields opaque to vm.
}

// Implement Default to allow take() to work during clear().
impl Default for ClassSource {
    fn default() -> Self {
        ClassSource::Internal
    }
}
