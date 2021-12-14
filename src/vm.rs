// analog to wren_vm.c from wren_c.

include!(concat!(env!("OUT_DIR"), "/wren_core_source.rs"));

use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::convert::{TryFrom, TryInto};
use std::error::Error;
use std::hash::Hasher;
use std::{str, usize};

use crate::compiler::{is_local_name, Arity, Constant, FnDebug, InputManager, Ops, Variable};
use crate::core::{init_base_classes, init_fn_and_fiber, register_core_primitives};
// use crate::ffi::c_api::WrenConfiguration;
// use crate::opt::random_bindings::{
//     random_bind_foreign_class, random_bind_foreign_method, random_source,
// };
use crate::wren::{Configuration, DebugLevel, ForeignMethodFn, LoadModuleResult, Slot};

use vmgc::heap::*;
use vmgc::object::*;
use vmgc::pointer::ObjectType;
use vmgc::types::GCError;

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
    FiberAbort(GlobalHandle<()>),
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

    fn as_try_return_value<'a>(&self, scope: &'a HandleScope) -> LocalHandle<'a, ()> {
        match self {
            VMError::Error(string) => scope.take(string.clone()).unwrap().erase_type(),
            VMError::FiberAbort(value) => scope.from_global(value),
        }
    }
}

impl From<GCError> for VMError {
    fn from(err: GCError) -> Self {
        VMError::Error(err.to_string())
    }
}

// In Wren false and null are false, everything else is true.
fn is_truthy(value: &HeapHandle<()>) -> bool {
    if value.is_null() {
        false
    } else if value.is_bool() {
        // FIXME: Could also add TryInto to &HeapHandle.
        value.clone().try_into().unwrap()
    } else {
        true
    }
}

fn is_falsey(value: &HeapHandle<()>) -> bool {
    !is_truthy(value)
}

// impl Hash for Value {
//     // See hashObject in wren_c wren_value.c
//             Value::String(v) => v.hash(state),
//             Value::Class(v) => v.borrow().name.hash(state),
//             Value::Range(v) => {
//                 hash_f64(v.borrow().from, state);
//                 hash_f64(v.borrow().to, state);
//                 // wren_c doesn't hash inclusive, but we could?
//             }
//     }
// }

// eq
//             (Value::Range(a_range), Value::Range(b_range)) => {
//                 let a = a_range.borrow();
//                 let b = b_range.borrow();
//                 a.from == b.from && a.to == b.to && a.is_inclusive == b.is_inclusive
//             }
//             (Value::String(a_string), Value::String(b_string)) => a_string.eq(&b_string),
//             (Value::Class(a), Value::Class(b)) => a.as_ptr() == b.as_ptr(),

pub(crate) struct Module {
    pub name: String,

    // Should this just be a map?  wren_utils.h suggests so?
    variables: List<()>,
    variable_names: Vec<String>,
}

impl Traceable for Module {
    fn trace(&mut self, visitor: &mut ObjectVisitor) {
        self.variables.trace(visitor);
    }
}

impl HostObject for Module {
    const TYPE_ID: ObjectType = ObjectType::Host;
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
        scope: &HandleScope,
        since_index: usize,
        f: F,
    ) -> Result<(), E>
    where
        F: Fn(&str, usize) -> Result<(), E>,
    {
        for i in since_index..self.variables.len() {
            let value = &self.variables[i];
            let name = &self.variable_names[i];
            // FIXME: Could we add try_downcast to HeapHandle<()>?
            let handle = scope.from_heap(value);
            let maybe_number: Option<LocalHandle<f64>> = handle.try_downcast();
            if let Some(number) = maybe_number {
                let num: f64 = number.into();
                f(name, num as usize)?;
            }
        }
        Ok(())
    }

    pub(crate) fn replace_implicit_definition(&mut self, symbol: u16, value: LocalHandle<()>) {
        let index = symbol as usize;
        assert!(self.variables[index].is_num());
        self.variables[index] = value.into();
    }

    pub(crate) fn add_variable(
        &mut self,
        name: &str,
        value: LocalHandle<()>,
    ) -> Result<u16, ModuleLimitError> {
        if self.variables.len() == MAX_MODULE_VARS {
            Err(ModuleLimitError::TooManyVariables)
        } else {
            self.variable_names.push(name.into());
            self.variables.push(value.into());
            Ok((self.variable_names.len() - 1) as u16)
        }
    }

    pub(crate) fn variable_by_name<'a>(
        &self,
        scope: &'a HandleScope,
        name: &str,
    ) -> Option<LocalHandle<'a, ()>> {
        self.lookup_symbol(name)
            .map(|index| scope.from_heap(&self.variables[index as usize]))
    }

    pub(crate) fn expect_class<'a>(
        &self,
        scope: &'a HandleScope,
        name: &str,
    ) -> LocalHandle<'a, ObjClass> {
        let symbol = self
            .lookup_symbol(name)
            .unwrap_or_else(|| panic!("failed to load {}", name));
        let local = scope.from_heap(&self.variables[symbol as usize]);
        local
            .try_downcast()
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
    fn from_error_value(value: LocalHandle<()>, stack_trace: StackTrace) -> RuntimeError {
        let maybe_msg = value
            .try_downcast()
            .map(|handle: LocalHandle<String>| handle.as_ref().clone());
        RuntimeError {
            // [error object] matches wren_c wrenDebugPrintStackTrace
            msg: maybe_msg.unwrap_or_else(|| "[error object]".to_string()),
            stack_trace,
        }
    }
}

// macro_rules! try_into {
//     ($func:ident,  $value_type:ident, $return_type:ident) => {
//         pub fn $func(&self) -> Option<Handle<$return_type>> {
//             match self {
//                 Value::$value_type(value) => Some(value.clone()),
//                 _ => None,
//             }
//         }
//     };
// }

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
    pc: usize,
    // The closure being executed.
    closure: HeapHandle<ObjClosure>,
    stack_start: usize,
}

impl Traceable for CallFrame {
    fn trace(&mut self, visitor: &mut ObjectVisitor) {
        self.closure.trace(visitor);
    }
}

impl CallFrame {
    // FIXME: Should this be heap handles?
    fn new(closure: HeapHandle<ObjClosure>, stack_start: usize) -> CallFrame {
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

struct OpenUpvalues {
    values: List<Upvalue>,
}

impl HostObject for OpenUpvalues {
    const TYPE_ID: ObjectType = ObjectType::Host;
}

impl Traceable for OpenUpvalues {
    fn trace(&mut self, visitor: &mut ObjectVisitor) {
        self.values.trace(visitor);
    }
}

impl OpenUpvalues {
    fn find_open_upvalue<'a>(
        &self,
        scope: &'a HandleScope,
        location: &StackOffset,
    ) -> Option<LocalHandle<'a, Upvalue>> {
        for heap_upvalue in &self.values {
            let upvalue = scope.from_heap(heap_upvalue);
            if upvalue.as_ref().has_location(location) {
                return Some(upvalue);
            }
        }
        None
    }

    fn push(&mut self, upvalue: &HeapHandle<Upvalue>) {
        assert!(upvalue.as_ref().is_open());
        self.values.push(upvalue.clone());
    }
}

pub struct ObjFiber {
    class_obj: HeapHandle<ObjClass>,
    pub(crate) error: HeapHandle<()>,
    pub(crate) caller: Option<HeapHandle<ObjFiber>>,
    // We can't grab at the call_stack to check if empty
    // while it might be held mutably for execution, so we cache
    // the "completed_normally" bool here.
    // FIXME: This is probably better as an enum?
    pub(crate) completed_normally_cache: bool,
    run_source: FiberRunSource,
    // FIXME: Should this be HeapHandle instead of RefCell?
    stack: RefCell<List<()>>,
    // Held in a RefCell so others can interact with the rest of
    // ObjFiber (to ask class, etc.) while the stack is  held mutably
    // for the executing fiber.
    call_stack: RefCell<Vec<CallFrame>>,
    open_upvalues: RefCell<OpenUpvalues>,
}

impl Traceable for ObjFiber {
    fn trace(&mut self, visitor: &mut ObjectVisitor) {
        self.class_obj.trace(visitor);
        self.error.trace(visitor);
        if let Some(caller) = &self.caller {
            caller.trace(visitor);
        }
        self.stack.borrow_mut().trace(visitor);
        for frame in self.call_stack.borrow_mut().iter_mut() {
            frame.trace(visitor);
        }
        self.open_upvalues.borrow_mut().trace(visitor);
    }
}

impl ObjFiber {
    pub fn just_started(&self) -> bool {
        let frames = self.call_stack.borrow();
        frames.len() == 1 && frames[0].pc == 0
    }

    pub fn is_root(&self) -> bool {
        matches!(self.run_source, FiberRunSource::Root)
    }

    pub fn is_try(&self) -> bool {
        matches!(self.run_source, FiberRunSource::Try)
    }

    fn return_from_fiber_take_caller<'a>(
        &mut self,
        scope: &'a HandleScope,
    ) -> Option<LocalHandle<'a, ObjFiber>> {
        let caller = self.caller.take();
        self.completed_normally_cache = self.call_stack.borrow().is_empty();
        caller.map(|handle| scope.from_heap(&handle))
    }

    // Can only be 0 or 1, enforced by Fiber.new.
    // Only valid to call when not running.
    fn arity(&self) -> u8 {
        // FIXME: Should as_ref() be allowed for HeapHandle?
        self.call_stack.borrow()[0]
            .closure
            .as_ref()
            .fn_obj
            .as_ref()
            .arity
            .as_u8()
    }

    fn push_call_arg_or_return_value(&mut self, arg: LocalHandle<()>) {
        if self.just_started() {
            // The fiber is being started for the first time. If its
            // function takes a parameter, bind an argument to it.
            // This exact check of arity == 1 is OK, because
            // Fiber.new also checks arity is either 0 or 1.
            if self.arity() == 1 {
                self.push(&arg.into());
            }
        } else {
            // The fiber is being resumed, make yield(), transfer()
            // or transferError() return the result.
            self.push(&arg.into());
        }
    }

    fn push_return_value(&mut self, arg: LocalHandle<()>) {
        // Push the argument to the fiber call, try or transfer
        // or the return value from a yield or transfer
        // onto the top-most frame of the callstack.
        self.push(&arg.into());
    }

    pub(crate) fn error(&self) -> &HeapHandle<()> {
        &self.error
    }

    pub fn has_error(&self) -> bool {
        !self.error.is_null()
    }

    fn index_for_stack_offset(&self, offset: StackOffset) -> usize {
        self.call_stack.borrow()[offset.frame_index].stack_start + offset.index
    }

    fn load<'a>(&self, scope: &'a HandleScope, loc: StackOffset) -> LocalHandle<'a, ()> {
        let index = self.index_for_stack_offset(loc);
        scope.from_heap(&self.stack.borrow()[index])
    }

    fn store(&self, loc: StackOffset, value: &HeapHandle<()>) {
        let index = self.index_for_stack_offset(loc);
        self.stack.borrow_mut()[index] = value.clone()
    }

    // FIXME: Should this take a Frame for bounds checking?
    #[inline] // 3% on map_numeric
    fn push(&self, value: &HeapHandle<()>) {
        self.stack.borrow_mut().push(value.clone());
    }

    // FIXME: No longer needs to be Result, now that Stack Underflow panics
    fn pop<'a>(&self, scope: &'a HandleScope) -> Result<LocalHandle<'a, ()>> {
        Ok(self.stack.borrow_mut().pop(scope).expect("Stack underflow"))
    }

    // FIXME: No longer needs to be Result, now that Stack Underflow panics
    // FIXME: This could return a &Value instead of Value, but the only
    // two callers who *do not* immediately clone() are And and Or ops.
    fn peek<'a>(&self, scope: &'a HandleScope) -> Result<LocalHandle<'a, ()>> {
        Ok(scope.from_heap(self.stack.borrow().last().expect("Stack underflow")))
    }

    fn load_this<'a>(&self, scope: &'a HandleScope, frame: &CallFrame) -> LocalHandle<'a, ()> {
        self.load_local(scope, frame, 0)
    }

    fn store_this(&self, frame: &CallFrame, value: LocalHandle<()>) {
        self.store_local(frame, 0, &value.into())
    }

    fn load_local<'a>(
        &self,
        scope: &'a HandleScope,
        frame: &CallFrame,
        offset: usize,
    ) -> LocalHandle<'a, ()> {
        // FIXME: bounds-check the number of locals based on compiler data?
        scope.from_heap(&self.stack.borrow()[frame.stack_start + offset])
    }

    fn store_local(&self, frame: &CallFrame, offset: usize, value: &HeapHandle<()>) {
        // FIXME: bounds-check the number of locals based on compiler data?
        self.stack.borrow_mut()[frame.stack_start + offset] = value.clone()
    }

    fn dump_stack(&self, frame: &CallFrame, active_module: &str, frame_depth: usize) {
        // Print the stack left (top) to right (bottom)
        let mut as_string = Vec::new();
        for (index, value) in self
            .stack
            .borrow()
            .iter()
            .skip(frame.stack_start)
            .enumerate()
            .rev()
        {
            let debug = format!("{:?}", value);
            as_string.push(format!("{}: {:10}", index, debug));
        }
        as_string.reverse();
        println!(
            "{:10}  Stack({:2}): {}",
            active_module,
            frame_depth,
            as_string.join(" ")
        );
    }
}

fn find_or_create_upvalue<'a>(
    scope: &'a HandleScope,
    fiber_ref: LocalHandle<'a, ObjFiber>,
    location: StackOffset,
) -> LocalHandle<'a, Upvalue> {
    let fiber = fiber_ref.as_ref();
    // wren_c uses a Value* for the search and the knowledge that values
    // are all held on a single stack/vector.
    // If the upvalue is still open, re-use that (so that modifications
    // are reflected in all callers).
    if let Some(upvalue) = fiber
        .open_upvalues
        .borrow()
        .find_open_upvalue(scope, &location)
    {
        return upvalue;
    }
    // If there isn't yet an upvalue (or there are, but they're closed but
    // running the function again, etc.), we make a new one.
    let upvalue = Upvalue::new(scope, fiber_ref, location);
    fiber
        .open_upvalues
        .borrow_mut()
        .push(&upvalue.clone().into());
    upvalue
}

impl Obj for ObjFiber {
    fn class_obj(&self) -> &HeapHandle<ObjClass> {
        &self.class_obj
    }
}

impl HostObject for ObjFiber {
    const TYPE_ID: ObjectType = ObjectType::Host;
}

impl ObjFiber {
    pub(crate) fn new<'a>(
        vm: &VM,
        scope: &'a HandleScope,
        closure: LocalHandle<ObjClosure>,
        run_source: FiberRunSource,
    ) -> LocalHandle<'a, ObjFiber> {
        let stack = scope.create::<List<()>>().unwrap();
        stack.as_mut().push(closure.clone().into());
        scope
            .take(ObjFiber {
                class_obj: scope
                    .as_ref(&vm.globals)
                    .fiber_class
                    .as_ref()
                    .unwrap()
                    .clone(),
                error: scope.create_null().into(),
                caller: None,
                run_source,
                completed_normally_cache: false,
                call_stack: RefCell::new(vec![CallFrame::new(closure.into(), 0)]),
                // FIXME: Should this be HeapHandle instead?
                stack: RefCell::new(stack.as_ref().clone()),
                open_upvalues: RefCell::new(OpenUpvalues {
                    values: List::default(),
                }),
            })
            .unwrap()
    }
}

impl core::fmt::Debug for ObjFiber {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let stack = self.call_stack.borrow();
        write!(f, "call_stack: (len {}), ", stack.len())
    }
}

// struct Api {
//     stack: GlobalHandle<List>,
//     error: GlobalHandle<()>,
// }

// impl Api {
//     fn new(slots: usize) -> Api {
//         Api {
//             stack: vec![Value::Null; slots],
//             error: Value::Null,
//         }
//     }

//     fn with_stack(stack: Vec<Value>) -> Api {
//         Api {
//             stack,
//             error: Value::Null,
//         }
//     }

//     fn ensure(&mut self, slots: usize) {
//         if slots >= self.stack.len() {
//             self.stack.resize(slots + 1, Value::Null);
//         }
//     }

//     fn into_return_value(self) -> Value {
//         // Take without copy.
//         self.stack.into_iter().next().unwrap()
//     }
// }

fn maybe_trace<T: HostObject>(maybe_object: &Option<HeapHandle<T>>, visitor: &mut ObjectVisitor) {
    if let Some(object) = maybe_object {
        object.trace(visitor)
    }
}
pub(crate) struct Globals {
    // Current executing Fiber (should eventually be a list?)
    pub(crate) fiber: Option<HeapHandle<ObjFiber>>,
    modules: HashMap<String, HeapHandle<Module>>,
    // The Core module is created first (empty)
    pub(crate) core_module: Option<HeapHandle<Module>>,
    // Then Class
    pub(crate) class_class: Option<HeapHandle<ObjClass>>,
    // Then Fn and Fiber (which are used by wren_core.wren)
    pub(crate) fn_class: Option<HeapHandle<ObjClass>>,
    pub(crate) fiber_class: Option<HeapHandle<ObjClass>>,
    pub(crate) last_imported_module: Option<HeapHandle<Module>>,
}

impl VM {
    fn fiber<'a>(&self, scope: &'a HandleScope) -> Option<LocalHandle<'a, ObjFiber>> {
        scope.from_maybe_heap(&scope.as_ref(&self.globals).fiber)
    }
}

impl HostObject for Globals {
    const TYPE_ID: ObjectType = ObjectType::Host;
}

impl Traceable for Globals {
    fn trace(&mut self, visitor: &mut ObjectVisitor) {
        maybe_trace(&self.fiber, visitor);
        for value in self.modules.values() {
            value.trace(visitor);
        }
        maybe_trace(&self.core_module, visitor);
        maybe_trace(&self.class_class, visitor);
        maybe_trace(&self.fn_class, visitor);
        maybe_trace(&self.fiber_class, visitor);
        maybe_trace(&self.last_imported_module, visitor);
    }
}

impl Globals {
    fn new<'a>(scope: &'a HandleScope) -> LocalHandle<'a, Globals> {
        scope
            .take(Globals {
                fiber: None,
                modules: HashMap::new(),
                core_module: None,
                class_class: None,
                fn_class: None,
                fiber_class: None,
                last_imported_module: None,
            })
            .unwrap()
    }
}

pub struct VM {
    // Single global symbol table for all method names (matches wren_c)
    // Separate Struct to allow easier passing to register_primitive
    pub methods: SymbolTable,

    // Finally the rest of wren_core.wren
    pub(crate) core: Option<GlobalHandle<CoreClasses>>,
    pub(crate) globals: GlobalHandle<Globals>,
    pub(crate) start_time: std::time::Instant,
    pub(crate) config: Configuration,
    // Args for the foreign function being called.
    // wren_c calls this apiStack
    // api: Option<Api>,
    // Hold onto the function pointers for the C API so we can
    // use rust function pointers in the normal config and just
    // look up the c ones here through the VM pointer.
    // pub(crate) c_config: WrenConfiguration,
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

impl Globals {
    pub fn has_module(&self, module_name: &str) -> bool {
        self.modules.contains_key(module_name)
    }

    pub fn has_variable(&self, module_name: &str, variable_name: &str) -> bool {
        if let Some(module) = self.modules.get(module_name) {
            // What if the module is mut_borrowed for execution?
            module.as_ref().lookup_symbol(variable_name).is_some()
        } else {
            // wren_c asserts in the case of the module not being defined.
            false
        }
    }
}

// Rust implementations backing the wren_c API.  Eventually these should
// shift to be the *rust* API and follow *rust* patterns.  All the C-isms
// should move into c_api.rs.
impl VM {
    // pub fn ensure_slots(&mut self, slots: usize) {
    //     match &mut self.api {
    //         None => self.api = Some(Api::new(slots)),
    //         Some(api) => api.ensure(slots),
    //     }
    // }

    // pub fn set_slot_string(&mut self, slot: Slot, string: &str) {
    //     assert!(self.api.is_some());
    //     if let Some(api) = &mut self.api {
    //         api.stack[slot] = Value::from_str(string);
    //     }
    // }

    // pub fn abort_fiber(&mut self, slot: Slot) {
    //     assert!(self.api.is_some());
    //     if let Some(api) = &mut self.api {
    //         api.error = api.stack[slot].clone();
    //     }
    // }

    // pub fn get_variable(
    //     &mut self,
    //     scope: &HandleScope,
    //     module_name: &str,
    //     variable_name: &str,
    //     slot: Slot,
    // ) {
    //     let value = if let Some(module) = self.modules.get(module_name) {
    //         // What if the module is mut_borrowed for execution?
    //         module
    //             .borrow()
    //             .variable_by_name(scope, variable_name)
    //             .unwrap()
    //             .clone()
    //     } else {
    //         // wren_c asserts in the case of the module not being defined.
    //         unreachable!("Module not defined.");
    //     };
    //     self.set_value_for_slot(slot, value);
    // }

    // pub fn set_slot_new_foreign(
    //     &mut self,
    //     slot: Slot,
    //     class_slot: Slot,
    //     user_data: Box<dyn UserData>,
    // ) {
    //     assert!(self.api.is_some());
    //     if let Some(api) = &mut self.api {
    //         let class = api.stack[class_slot]
    //             .try_into_class()
    //             .expect("Slot must hold a class.");
    //         assert!(matches!(class.borrow().source, ClassSource::Foreign));
    //         api.stack[slot] = Value::Foreign(new_handle(ObjForeign::new(class, user_data)))
    //     }
    // }

    // pub fn slot_count(&self) -> usize {
    //     match &self.api {
    //         None => 0,
    //         Some(api) => api.stack.len(),
    //     }
    // }

    // pub fn set_slot_null(&mut self, slot: Slot) {
    //     assert!(self.api.is_some());
    //     if let Some(api) = &mut self.api {
    //         api.stack[slot] = Value::Null;
    //     }
    // }

    // pub fn get_slot_double(&mut self, slot: Slot) -> f64 {
    //     assert!(self.api.is_some());
    //     self.api.as_ref().unwrap().stack[slot]
    //         .try_into_num()
    //         .expect("slot is not a num")
    // }

    // pub fn set_slot_double(&mut self, slot: Slot, num: f64) {
    //     assert!(self.api.is_some());
    //     if let Some(api) = &mut self.api {
    //         api.stack[slot] = Value::Num(num)
    //     }
    // }

    // pub fn set_slot_new_list(&mut self, slot: Slot) {
    //     assert!(self.api.is_some());
    //     let value = Value::List(self.new_list(vec![]));
    //     if let Some(api) = &mut self.api {
    //         api.stack[slot] = value;
    //     }
    // }

    // pub fn get_list_count(&mut self, slot: Slot) -> usize {
    //     assert!(self.api.is_some());
    //     let list = self.value_for_slot(slot).try_into_list().unwrap();
    //     let count = list.borrow().len();
    //     count
    // }

    // pub fn get_list_element(&mut self, list_slot: Slot, index: usize, element_slot: Slot) {
    //     assert!(self.api.is_some());
    //     let list_ref = self.value_for_slot(list_slot).try_into_list().unwrap();
    //     let list = list_ref.borrow();
    //     self.set_value_for_slot(element_slot, list.elements[index].clone())
    // }

    // pub fn set_list_element(&mut self, list_slot: Slot, index: usize, element_slot: Slot) {
    //     assert!(self.api.is_some());
    //     let list = self.value_for_slot(list_slot).try_into_list().unwrap();
    //     let element = self.value_for_slot(element_slot);
    //     list.borrow_mut().elements[index] = element.clone();
    // }

    // pub fn insert_in_list(&mut self, list_slot: Slot, index: usize, element_slot: Slot) {
    //     assert!(self.api.is_some());
    //     let list = self.value_for_slot(list_slot).try_into_list().unwrap();
    //     let element = self.value_for_slot(element_slot);
    //     list.borrow_mut().elements.insert(index, element.clone());
    // }

    // pub fn set_slot_new_map(&mut self, slot: Slot) {
    //     assert!(self.api.is_some());
    //     let value = Value::Map(self.new_map());
    //     if let Some(api) = &mut self.api {
    //         api.stack[slot] = value;
    //     }
    // }

    // pub fn map_contains_key(&self, map_slot: Slot, key_slot: Slot) -> bool {
    //     assert!(self.api.is_some());
    //     let map = self
    //         .value_for_slot(map_slot)
    //         .try_into_map()
    //         .expect("slot is not a map");
    //     let key = self.value_for_slot(key_slot);
    //     let contains = map.borrow().contains_key(key);
    //     contains
    // }

    // pub fn get_map_value(&mut self, map_slot: Slot, key_slot: Slot, value_slot: Slot) {
    //     assert!(self.api.is_some());
    //     let map = self
    //         .value_for_slot(map_slot)
    //         .try_into_map()
    //         .expect("slot is not a map");
    //     let key = self.value_for_slot(key_slot);
    //     let value = map.borrow().data[key].clone();
    //     self.set_value_for_slot(value_slot, value)
    // }

    // pub fn set_map_value(&self, map_slot: Slot, key_slot: Slot, value_slot: Slot) {
    //     assert!(self.api.is_some());
    //     let map = self
    //         .value_for_slot(map_slot)
    //         .try_into_map()
    //         .expect("slot is not a map");
    //     let key = self.value_for_slot(key_slot).clone();
    //     let value = self.value_for_slot(value_slot).clone();
    //     map.borrow_mut().data.insert(key, value);
    // }

    // pub fn remove_map_value(&mut self, map_slot: Slot, key_slot: Slot, removed_value_slot: Slot) {
    //     assert!(self.api.is_some());
    //     let map = self
    //         .value_for_slot(map_slot)
    //         .try_into_map()
    //         .expect("slot is not a map");
    //     let key = self.value_for_slot(key_slot);
    //     let value = map.borrow_mut().data.remove(key).unwrap_or(Value::Null);
    //     self.set_value_for_slot(removed_value_slot, value);
    // }

    // pub fn get_slot_bool(&mut self, slot: Slot) -> bool {
    //     assert!(self.api.is_some());
    //     self.api.as_ref().unwrap().stack[slot]
    //         .try_into_bool()
    //         .expect("slot is not a bool")
    // }

    // pub fn set_slot_bool(&mut self, slot: Slot, value: bool) {
    //     assert!(self.api.is_some());
    //     if let Some(api) = &mut self.api {
    //         api.stack[slot] = Value::Boolean(value)
    //     }
    // }

    // // This isn't quite right for the rust API, but is used by the C API.
    // pub(crate) fn call_handle_for_signature(&mut self, signature: &str) -> Value {
    //     let symbol = self.methods.ensure_symbol(signature);
    //     // Create a little stub function that assumes the arguments are on the
    //     // stack and calls the method.
    //     let fn_obj = ObjFn::stub_call(self, signature, symbol);
    //     let closure = ObjClosure::new(self, new_handle(fn_obj));
    //     Value::Closure(new_handle(closure))
    // }

    // // This is just a (non-functional) stub for the C API as well.
    // pub(crate) fn call(&mut self, _method: &Value) -> InterpretResult {
    //     self.set_value_for_slot(0, Value::Null);
    //     InterpretResult::Success
    // }

    // // Allows c_api to write addition APIs which the rust public API
    // // may not also share.
    // pub(crate) fn value_for_slot(&self, slot: Slot) -> &Value {
    //     &self.api.as_ref().unwrap().stack[slot]
    // }

    // pub(crate) fn set_value_for_slot(&mut self, slot: Slot, value: Value) {
    //     assert!(self.api.is_some());
    //     if let Some(api) = &mut self.api {
    //         api.stack[slot] = value;
    //     }
    // }

    // // Maybe return Option<SlotType> and None on failure instead of panick?
    // pub fn type_for_slot(&self, slot: Slot) -> SlotType {
    //     match self.value_for_slot(slot) {
    //         Value::Boolean(_) => SlotType::Bool,
    //         Value::Num(_) => SlotType::Num,
    //         Value::Foreign(_) => SlotType::Foreign,
    //         Value::List(_) => SlotType::List,
    //         Value::Map(_) => SlotType::Map,
    //         Value::Null => SlotType::Null,
    //         Value::String(_) => SlotType::String,
    //         _ => SlotType::Unknown,
    //     }
    // }
}

// impl Clear for Module {
//     fn clear(&mut self) {
//         self.variables.clear();
//         self.variable_names.clear();
//     }
// }

// impl Clear for ObjFiber {
//     fn clear(&mut self) {
//         self.call_stack.borrow_mut().clear();
//     }
// }

// impl Clear for ObjClass {
//     fn clear(&mut self) {
//         clear_class(self.class.take());
//         clear_class(self.superclass.take());
//         self.methods.clear();
//     }
// }

// fn clear_class(maybe_class: Option<Handle<ObjClass>>) {
//     if let Some(class) = maybe_class {
//         class.take().clear();
//     }
// }

// impl Clear for ObjInstance {
//     fn clear(&mut self) {
//         self.class_obj.borrow_mut().clear();
//         self.fields.clear();
//     }
// }

// impl Clear for ObjForeign {
//     fn clear(&mut self) {
//         self.class_obj.borrow_mut().clear();
//     }
// }

// impl Clear for ObjList {
//     fn clear(&mut self) {
//         self.elements.clear();
//     }
// }

// impl Clear for ObjMap {
//     fn clear(&mut self) {
//         self.data.clear();
//     }
// }

// impl Clear for ObjRange {
//     fn clear(&mut self) {
//         // No object references held.
//     }
// }

// impl Clear for ObjFn {
//     fn clear(&mut self) {
//         self.class_obj.borrow_mut().clear();
//         self.constants.clear();
//         self.code.clear();
//         self.module.borrow_mut().clear();
//     }
// }

// impl Clear for ObjClosure {
//     fn clear(&mut self) {
//         self.class_obj.borrow_mut().clear();
//         self.fn_obj.borrow_mut().clear();
//     }
// }

// fn clear_maybe_module(maybe_module: Option<Handle<Module>>) {
//     if let Some(module) = maybe_module {
//         module.borrow_mut().clear();
//     }
// }

// impl Drop for VM {
//     fn drop(&mut self) {
//         // Tear down all fibers, including all stacks.
//         // Eventually code will be able to hold onto fibers.
//         if let Some(fiber) = &self.fiber {
//             fiber.borrow_mut().clear();
//         }

//         // Modules keep references to functions
//         // functions keep references to modules.
//         clear_maybe_module(self.core_module.take());
//         clear_maybe_module(self.last_imported_module.take());
//         for module in self.modules.values() {
//             module.borrow_mut().clear();
//         }

//         // Classes hold onto Methods, which include Functions.
//         clear_class(self.class_class.take());
//         clear_class(self.fn_class.take());
//         clear_class(self.fiber_class.take());

//         if let Some(core) = self.core.take() {
//             core.num.borrow_mut().clear();
//             core.bool_class.borrow_mut().clear();
//             core.null.borrow_mut().clear();
//             core.string.borrow_mut().clear();
//             core.range.borrow_mut().clear();
//             core.list.borrow_mut().clear();
//             core.map.borrow_mut().clear();
//         }
//     }
// }

impl core::fmt::Debug for VM {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        // let scope = HandleScope::new(self.heap);
        write!(f, "VM {{ ")?;
        // let globals = scope.as_ref(&self.globals);
        // if let Some(fiber) = &globals.fiber {
        //     write!(f, "stack: {:?}, ", fiber.as_ref())?;
        // }
        write!(f, "methods: (len {}) ", self.methods.names.len())?;
        write!(f, "}}")
    }
}

impl VM {
    pub(crate) fn new_list<'a>(
        &self,
        scope: &'a HandleScope,
        contents: Vec<LocalHandle<()>>,
    ) -> LocalHandle<'a, ObjList> {
        scope
            .take(ObjList {
                class_obj: scope.as_ref(self.core.as_ref().unwrap()).list.clone(),
                elements: contents.into(),
            })
            .unwrap()
    }

    // FIXME: How to share code?
    pub(crate) fn new_list_from_heap<'a>(
        &self,
        scope: &'a HandleScope,
        contents: Vec<HeapHandle<()>>,
    ) -> LocalHandle<'a, ObjList> {
        scope
            .take(ObjList {
                class_obj: scope.as_ref(self.core.as_ref().unwrap()).list.clone(),
                elements: contents.into(),
            })
            .unwrap()
    }

    pub(crate) fn new_map<'a>(&self, scope: &'a HandleScope) -> LocalHandle<'a, ObjMap> {
        scope
            .take(ObjMap {
                class_obj: scope.as_ref(self.core.as_ref().unwrap()).map.clone(),
                data: HashMap::new(),
            })
            .unwrap()
    }

    pub(crate) fn new_range<'a>(
        &self,
        scope: &'a HandleScope,
        from: f64,
        to: f64,
        is_inclusive: bool,
    ) -> LocalHandle<'a, ObjRange> {
        scope
            .take(ObjRange {
                class_obj: scope.as_ref(self.core.as_ref().unwrap()).range.clone(),
                from,
                to,
                is_inclusive,
            })
            .unwrap()
    }

    pub(crate) fn new_fiber<'a>(
        &self,
        scope: &'a HandleScope,
        closure: LocalHandle<ObjClosure>,
    ) -> LocalHandle<'a, ObjFiber> {
        ObjFiber::new(self, scope, closure, FiberRunSource::Other)
    }

    pub(crate) fn new_class<'a>(
        &self,
        scope: &'a HandleScope,
        superclass: LocalHandle<ObjClass>,
        source: ClassSource,
        name: String,
    ) -> Result<LocalHandle<'a, ObjClass>> {
        let class_class =
            scope.from_heap(scope.as_ref(&self.globals).class_class.as_ref().unwrap());
        // Create the metaclass.
        let metaclass_name_string = format!("{} metaclass", name);
        // let metaclass_name = Value::from_string(metaclass_name_string);

        let metaclass = new_single_class(scope, ClassSource::Internal, metaclass_name_string);
        metaclass.as_mut().class = Some(class_class.clone().into());

        // Metaclasses always inherit Class and do not parallel the non-metaclass
        // hierarchy.
        metaclass.as_mut().bind_superclass(class_class);

        let class = new_single_class(scope, source, name);
        class.as_mut().class = Some(metaclass.into());
        class.as_mut().bind_superclass(superclass);

        Ok(class)
    }
}

impl ObjClass {
    pub(crate) fn bind_superclass(&mut self, superclass: LocalHandle<ObjClass>) {
        self.superclass = Some(superclass.clone().into());

        // Include the superclass in the total number of fields.
        match &mut self.source {
            ClassSource::Source(num_fields) => {
                *num_fields += superclass.as_ref().num_fields().unwrap_or(0);
            }
            _ => match superclass.as_ref().num_fields() {
                Some(num_fields) if num_fields > 0 => {
                    panic!("A foreign class cannot inherit from a class with fields.");
                }
                _ => {}
            },
        }

        // Inherit methods from its superclass.
        // FIXME: Should this be in reverse order (to minimize # of resizes?)
        self.inherit_methods_from(superclass.as_ref());
    }
}

fn new_single_class<'a>(
    scope: &'a HandleScope,
    source: ClassSource,
    name: String,
) -> LocalHandle<'a, ObjClass> {
    // the wren_c version does a lot more?  Unclear if this should.
    scope
        .take(ObjClass {
            name,
            methods: Vec::new(),
            class: None,
            superclass: None,
            source,
        })
        .unwrap()
}

fn validate_superclass<'a>(
    name: &str,
    superclass_value: LocalHandle<'a, ()>,
    source: &ClassSource,
) -> Result<LocalHandle<'a, ObjClass>> {
    // Make sure the superclass is a class.
    let superclass: LocalHandle<ObjClass> = superclass_value.try_downcast().ok_or_else(|| {
        VMError::from_string(format!(
            "Class '{}' cannot inherit from a non-class object.",
            name
        ))
    })?;

    // In wren_c, this is required since wren_c does blind-casts
    // of "this" in primitives.  safe_wren also does unwrap() and would
    // (safely) panic if "this" were a ObjInstance subclass.
    // FIXME: Merge with match below by checking ClassSource::Internal?
    match &superclass.as_ref().name[..] {
        "Class" | "Fiber" | "Fn" | "List" | "Map" | "Range" | "String" | "Bool" | "Null"
        | "Num" => {
            return Err(VMError::from_string(format!(
                "Class '{}' cannot inherit from built-in class '{}'.",
                name,
                superclass.as_ref().name
            )));
        }
        _ => {}
    }
    match (source, &superclass.as_ref().source) {
        (_, ClassSource::Foreign) => {
            return Err(VMError::from_string(format!(
                "Class '{}' cannot inherit from foreign class '{}'.",
                name,
                superclass.as_ref().name
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
            if fields + super_fields > MAX_FIELDS {
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

    // if module.name.eq("random") {
    //     let methods = random_bind_foreign_class(vm, &module.name, &class.name);
    //     class.set_method(allocate_symbol, Method::ForeignFunction(methods.allocate));
    //     // if let Some(finalize) = methods.finalize {
    //     //     class.set_method(finalize_symbol, Method::ForeignFunction(finalize));
    //     // }
    // }
}

fn create_class<'a>(
    vm: &mut VM,
    scope: &'a HandleScope,
    fiber: &ObjFiber,
    source: ClassSource,
    module: &Module,
) -> Result<()> {
    // Pull the name and superclass off the stack.
    let superclass_value = fiber.pop(scope)?;
    let name_value = fiber.pop(scope)?;

    let name = name_value
        .try_downcast()
        .map(|handle: LocalHandle<String>| handle.as_ref().clone())
        .ok_or_else(|| VMError::from_str("Class name not string."))?;
    let superclass = validate_superclass(&name, superclass_value, &source)?;

    let class = vm.new_class(scope, superclass, source.clone(), name)?;
    if let ClassSource::Foreign = source {
        bind_foreign_class(vm, class.as_mut(), module)
    }
    // After bind_foreign_class to avoid a clone, should not make a differnce.
    fiber.push(&class.erase_type().into());
    Ok(())
}

// type Handle<T> = Rc<RefCell<T>>;

// pub(crate) fn new_handle<T>(t: T) -> Handle<T> {
//     Rc::new(RefCell::new(t))
// }

#[derive(Debug)]
pub(crate) struct CoreClasses {
    pub(crate) num: HeapHandle<ObjClass>,
    pub(crate) bool_class: HeapHandle<ObjClass>,
    pub(crate) null: HeapHandle<ObjClass>,
    pub(crate) string: HeapHandle<ObjClass>,
    pub(crate) range: HeapHandle<ObjClass>,
    pub(crate) list: HeapHandle<ObjClass>,
    pub(crate) map: HeapHandle<ObjClass>,
}

impl HostObject for CoreClasses {
    const TYPE_ID: ObjectType = ObjectType::Host;
}

impl Traceable for CoreClasses {
    fn trace(&mut self, visitor: &mut ObjectVisitor) {
        self.num.trace(visitor);
        self.bool_class.trace(visitor);
        self.null.trace(visitor);
        self.string.trace(visitor);
        self.range.trace(visitor);
        self.list.trace(visitor);
        self.map.trace(visitor);
    }
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
        value: LocalHandle<()>,
    ) -> Result<u16, DefinitionError> {
        // See if the variable is already explicitly or implicitly declared.
        match self.lookup_symbol(name) {
            None => {
                // New variable!
                self.add_variable(name, value).map_err(From::from)
            }
            Some(symbol) => {
                let existing_value = &self.variables[symbol as usize];
                let maybe_line: Option<f64> = existing_value.clone().try_into().ok();
                if let Some(line) = maybe_line {
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
pub(crate) fn define_class<'a>(
    scope: &'a HandleScope,
    module: &mut Module,
    name: &str,
) -> LocalHandle<'a, ObjClass> {
    let class = scope
        .take(ObjClass {
            name: name.into(),
            methods: Vec::new(),
            class: None,
            superclass: None,
            source: ClassSource::Internal,
        })
        .unwrap();

    module
        .define_variable(name, class.erase_type())
        .expect("defined");
    class
}

impl VM {
    pub(crate) fn load_wren_core<'a>(&mut self, scope: &'a HandleScope, module_name: &str) {
        // wren_core_source() is generated by build.rs from wren_core.wren
        let source = wren_core_source();
        let input = InputManager::from_str(source);
        let closure = self
            .compile_in_module(scope, module_name, input)
            .expect("compile wren_core");
        // debug_bytecode(vm, &closure.borrow(), module);
        self.run(scope, closure).expect("run wren_core");
    }
}

enum FunctionNext<'a> {
    // FIXME: Use LocalHandle with the right lifetime?
    Call(LocalHandle<'a, ObjClosure>, usize),
    Return(LocalHandle<'a, ()>),
    FiberAction(FiberAction<'a>),
    CaptureUpvalues(LocalHandle<'a, ObjClosure>, Vec<crate::compiler::Upvalue>),
    CloseUpvalues(StackOffset),
}

// FIXME: Unclear if this is needed now that we have a single unified stack!
#[derive(Copy, Clone, Debug)]
struct StackOffset {
    // CAUTION: It's very easy to create this with a wrong index.
    frame_index: usize,
    // index is assumed to be relative, to the stack_start
    // which is referenced in the frame refered to by the frame_index.
    index: usize,
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

enum UpvalueStorage {
    Open(HeapHandle<ObjFiber>, StackOffset),
    Closed(HeapHandle<()>),
}

struct Upvalue {
    storage: UpvalueStorage,
}

impl HostObject for Upvalue {
    const TYPE_ID: ObjectType = ObjectType::Host;
}

impl Traceable for Upvalue {
    fn trace(&mut self, visitor: &mut ObjectVisitor) {
        match &self.storage {
            UpvalueStorage::Open(fiber, _) => fiber.trace(visitor),
            UpvalueStorage::Closed(value) => value.trace(visitor),
        }
    }
}

impl Upvalue {
    fn new<'a>(
        scope: &'a HandleScope,
        fiber: LocalHandle<ObjFiber>,
        location: StackOffset,
    ) -> LocalHandle<'a, Upvalue> {
        scope
            .take(Upvalue {
                storage: UpvalueStorage::Open(fiber.into(), location),
            })
            .unwrap()
    }

    fn is_open(&self) -> bool {
        matches!(&self.storage, UpvalueStorage::Open(_, _))
    }

    fn has_location(&self, location: &StackOffset) -> bool {
        matches!(&self.storage, UpvalueStorage::Open(_, l) if l.eq(location))
    }

    fn location(&self) -> Option<StackOffset> {
        match &self.storage {
            UpvalueStorage::Open(_, l) => Some(*l),
            UpvalueStorage::Closed(_) => None,
        }
    }

    fn load<'a>(&self, scope: &'a HandleScope) -> LocalHandle<'a, ()> {
        match &self.storage {
            // FIXME: Can we avoid making a LocalHandle for Fiber on every load?
            UpvalueStorage::Open(fiber, loc) => scope.from_heap(fiber).as_ref().load(scope, *loc),
            UpvalueStorage::Closed(value) => scope.from_heap(value),
        }
    }

    fn store(&mut self, new_value: &HeapHandle<()>) {
        match &mut self.storage {
            UpvalueStorage::Open(fiber, loc) => fiber.as_mut().store(*loc, new_value),
            UpvalueStorage::Closed(value) => *value = new_value.clone(),
        }
    }
}

impl ObjFiber {
    // Closes any open upvalues that have been created for stack slots at [last]
    // and above.
    // The "multi-close" ability is only used by "return".  This is only needed
    // because "return" is also used as "yield" in wren.  Thus we want the
    // ability to "yield" (creating upvalues) without popping them as one
    // might expect from "return" (and is how close_upvalues) is used one at
    // a time in the going-out-of-scope case by the compiler.
    // Where this is called, "fiber" is already borrowed, so we
    // can't easily mut-borrow fiber, thus this is &self, and we have a RefCell
    // around OpenUpvalues.
    fn close_upvalues_at_or_above(&self, scope: &HandleScope, location: StackOffset) {
        let close_upvalue_if_above = |u: &mut HeapHandle<Upvalue>| {
            // All open values have a location.
            let l = u.as_ref().location().unwrap();
            if l >= location {
                // Move the value into the upvalue itself and point the upvalue to it.
                let value = self.load(scope, l);
                u.as_mut().storage = UpvalueStorage::Closed(value.into());
                true // Remove it from the open upvalue list.
            } else {
                false
            }
        };

        // drain_filter is still not stable?
        // https://github.com/rust-lang/rust/issues/43244
        // self.open_upvalues.drain_filter(close_upvalue_if_above);

        let vec = &mut self.open_upvalues.borrow_mut().values;
        let mut i = 0;
        while i < vec.len() {
            if close_upvalue_if_above(&mut vec[i]) {
                vec.remove(scope, i);
            } else {
                i += 1;
            }
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

    // if module_name.eq("random") {
    //     return Some(random_bind_foreign_method(
    //         vm, class_name, is_static, signature,
    //     ));
    // }
    // FIXME: support opt_meta
    None
}

fn bind_method_code(scope: &HandleScope, class: &ObjClass, fn_obj: &mut ObjFn) {
    // Shift this class's fields down past the inherited ones. We don't
    // check for overflow here because we'll see if the number of fields
    // overflows when the subclass is created.
    fn field_adjustment(class: &ObjClass) -> usize {
        class
            .superclass
            .as_ref()
            .unwrap()
            .borrow()
            .num_fields()
            .unwrap_or(0)
    }

    for op in &mut fn_obj.code {
        match op {
            Ops::CallSuper(_, _, super_constant) => {
                let value = class.superclass.as_ref().unwrap().erase_type();
                // Making this a function call triggers a second mut borrow. :/
                // fn_obj.set_constant(super_constant, value);
                fn_obj.constants[super_constant.as_index()] = value;
            }
            Ops::LoadField(field) => *field += field_adjustment(class),
            Ops::StoreField(field) => *field += field_adjustment(class),
            Ops::Closure(constant, _) => {
                // Bind the nested closure too.
                // Note: If HeapHandle had try_downcast, we would not need to
                // pass a HandleScope into this function.
                let fn_obj: LocalHandle<ObjFn> = scope
                    .from_heap(&fn_obj.constants[constant.as_index()])
                    .try_downcast()
                    .unwrap();
                bind_method_code(scope, class, fn_obj.borrow_mut());
            }
            _ => {}
        };
    }
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
    scope: &HandleScope,
    is_static: bool,
    symbol: Symbol,
    module: &Module,
    class: &mut ObjClass,
    method_value: LocalHandle<()>,
) -> Result<()> {
    let method = {
        let maybe_sig: Option<LocalHandle<String>> = method_value.try_downcast();
        if let Some(signature) = maybe_sig {
            let module_name = &module.name;
            let class_name = &class.name;
            let foreign_fn =
                find_foreign_method(vm, module_name, class_name, is_static, signature.as_ref())
                    .ok_or_else(|| {
                        VMError::from_string(format!(
                            "Could not find foreign method '{}' for class {} in module '{}'.",
                            signature.as_ref(),
                            class_name,
                            module_name
                        ))
                    })?;
            Method::ForeignFunction(foreign_fn)
        } else if let Some(handle) = method_value.try_downcast() {
            let closure_handle: LocalHandle<ObjClosure> = handle;
            // Patch up the bytecode now that we know the superclass.
            let closure: &ObjClosure = closure_handle.as_mut();
            bind_method_code(scope, &class, closure.fn_obj.as_mut());
            Method::Block(closure_handle.into())
        } else {
            // Is this a compiler error?  Should this panic?
            return Err(VMError::from_str(
                "method value not a string (foreign) or closure (block",
            ));
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

enum ImportResult<'a> {
    Existing(LocalHandle<'a, Module>),
    New(LocalHandle<'a, ObjClosure>),
}

fn try_load_module(vm: &mut VM, name: &str) -> Option<LoadModuleResult> {
    if let Some(load_module) = vm.config.load_module_fn {
        if let Some(result) = load_module(vm, name) {
            return Some(result);
        }
    }
    // if name.eq("random") {
    //     return Some(LoadModuleResult {
    //         source: random_source(),
    //     });
    // }
    None
}

fn import_module<'a>(
    vm: &mut VM,
    scope: &'a HandleScope,
    importer_name: &str,
    unresolved_name: &str,
) -> Result<ImportResult<'a>> {
    let name = resolve_module(vm, importer_name, unresolved_name);

    // If the module is already loaded, we don't need to do anything.
    if let Some(m) = scope.as_ref(&vm.globals).modules.get(&name) {
        return Ok(ImportResult::Existing(scope.from_heap(m)));
    }

    let result = try_load_module(vm, &name)
        .ok_or_else(|| VMError::from_string(format!("Could not load module '{}'.", name)))?;

    let input = InputManager::from_string(result.source);
    let closure = vm
        .compile_in_module(&scope, &name, input)
        .map_err(|_| VMError::from_string(format!("Could not compile module '{}'.", name)))?;

    // Return the closure that executes the module.
    Ok(ImportResult::New(closure))
}

// Unclear if this needs to be a separate function?
fn get_module_variable<'a>(
    scope: &'a HandleScope,
    module: &Module,
    variable_name: &str,
) -> Result<LocalHandle<'a, ()>> {
    module
        .variable_by_name(scope, variable_name)
        .ok_or_else(|| {
            VMError::from_string(format!(
                "Could not find a variable named '{}' in module '{}'.",
                variable_name, module.name
            ))
        })
}

impl VM {
    pub fn new(scope: &HandleScope, config: Configuration) -> Self {
        let mut vm = Self {
            // Invalid import name, intentionally.
            methods: SymbolTable::default(),
            globals: GlobalHandle::from(Globals::new(&scope)),
            start_time: std::time::Instant::now(),
            config,
            core: None,
            // api: None,
            // c_config: WrenConfiguration::default(),
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
            variables: List::default(),
            variable_names: Vec::new(),
        };
        init_base_classes(scope, &mut vm, &mut stub_core);
        scope.as_mut(&vm.globals).class_class =
            Some(stub_core.expect_class(&scope, "Class").into());
        init_fn_and_fiber(&mut vm, &scope, &mut stub_core);
        scope.as_mut(&vm.globals).core_module = Some(scope.take(stub_core).unwrap().into());
        let core_name = CORE_MODULE_NAME;
        vm.load_wren_core(&scope, core_name);
        scope.as_mut(&vm.globals).core_module =
            Some(scope.as_mut(&vm.globals).modules.remove(core_name).unwrap());
        register_core_primitives(&scope, &mut vm);
        vm
    }

    pub(crate) fn lookup_or_register_empty_module<'a>(
        &mut self,
        scope: &'a HandleScope,
        name: &str,
    ) -> LocalHandle<'a, Module> {
        if let Some(m) = scope.as_ref(&self.globals).modules.get(name) {
            return scope.from_heap(m);
        }
        let module = self.new_module_with_name(scope, name);
        self.register_module(scope, module.clone());
        module
    }

    pub(crate) fn new_module_with_name<'a>(
        &self,
        scope: &'a HandleScope,
        name: &str,
    ) -> LocalHandle<'a, Module> {
        // We automatically import Core into all modules.
        // Implicitly import the core module.
        let core = scope.from_heap(scope.as_ref(&self.globals).core_module.as_ref().unwrap());
        scope
            .take(Module {
                name: name.into(),
                // FIXME: These clone the handle, not the contents!
                variable_names: core.as_ref().variable_names.clone(),
                variables: core.as_ref().variables.clone(),
            })
            .unwrap()
    }

    pub(crate) fn register_module<'a>(
        &mut self,
        scope: &'a HandleScope,
        module: LocalHandle<'a, Module>,
    ) {
        let name = module.as_ref().name.clone();
        scope
            .as_mut(&self.globals)
            .modules
            .insert(name, module.into());
    }

    // called wrenGetClass in wren_c.
    pub(crate) fn class_for_value<'a>(
        &self,
        scope: &'a HandleScope,
        value_heap: &HeapHandle<()>,
    ) -> LocalHandle<'a, ObjClass> {
        // try_as_ref doesn't exist on HeapHandle, so we use a LocalHandle.
        let value = scope.from_heap(value_heap);
        let core = scope.as_ref(self.core.as_ref().unwrap());
        if value.is_null() {
            scope.from_heap(&core.null)
        } else if value.is_num() {
            scope.from_heap(&core.num)
        } else if value.is_bool() {
            scope.from_heap(&core.bool_class)
        } else if let Some(_) = value.try_as_ref::<String>() {
            scope.from_heap(&core.string)
            // FIXME: There should be a better way to handle this without
            // an if-cascade.  Can we cast to dyn Obj instead?
        } else if let Some(obj) = value.try_as_ref::<ObjList>() {
            scope.from_heap(obj.class_obj())
        } else if let Some(obj) = value.try_as_ref::<ObjMap>() {
            scope.from_heap(obj.class_obj())
        } else if let Some(obj) = value.try_as_ref::<ObjRange>() {
            scope.from_heap(obj.class_obj())
        } else if let Some(obj) = value.try_as_ref::<ObjFiber>() {
            scope.from_heap(obj.class_obj())
        } else if let Some(obj) = value.try_as_ref::<ObjClass>() {
            scope.from_heap(obj.class_obj())
        } else if let Some(obj) = value.try_as_ref::<ObjInstance>() {
            scope.from_heap(obj.class_obj())
        } else if let Some(obj) = value.try_as_ref::<ObjForeign>() {
            scope.from_heap(obj.class_obj())
        } else if let Some(obj) = value.try_as_ref::<ObjClosure>() {
            scope.from_heap(obj.class_obj())
        } else if let Some(obj) = value.try_as_ref::<ObjFn>() {
            scope.from_heap(obj.class_obj())
        } else {
            unimplemented!()
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
            let closure = frame.closure.as_ref();
            let fn_obj = closure.fn_obj.as_ref();
            let module = fn_obj.module.as_ref().name.clone();
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

    // fn call_foreign(&mut self, foreign: &ForeignMethodFn, args: Vec<Value>) -> Result<Value> {
    //     assert!(self.api.is_none(), "Cannot already be in foreign call.");
    //     self.api = Some(Api::with_stack(args));
    //     foreign(self);
    //     let api = self.api.take().unwrap();
    //     if api.error.is_null() {
    //         Ok(api.into_return_value())
    //     } else {
    //         Err(VMError::FiberAbort(api.error))
    //     }
    // }

    fn cascade_error(
        &mut self,
        scope: &HandleScope,
        error: LocalHandle<()>,
    ) -> Result<(), RuntimeError> {
        let stack_trace_fiber = self.fiber(scope).unwrap();
        loop {
            let callee = self.fiber(scope).unwrap();
            // Set Fiber.error on the current fiber. Can't do this
            // deeper in the stack because can't borrow_mut there.
            callee.borrow_mut().error = error.into();
            // If we have a caller, it's now the new fiber.
            let caller = match self.fiber(scope) {
                Some(fiber) => fiber.as_mut().return_from_fiber_take_caller(scope),
                _ => None,
            };
            scope.as_mut(&self.globals).fiber = caller.map(|local| local.into());

            match self.fiber(scope) {
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

    pub(crate) fn run<'a>(
        &mut self,
        scope: &'a HandleScope,
        closure: LocalHandle<ObjClosure>,
    ) -> Result<LocalHandle<'a, ()>, RuntimeError> {
        scope.as_mut(&self.globals).fiber =
            Some(ObjFiber::new(self, &scope, closure, FiberRunSource::Root).into());
        loop {
            let result = self.run_in_fiber(scope, self.fiber(scope).unwrap());
            match result {
                Ok(FiberAction::Call(fiber, arg)) => {
                    fiber.as_mut().caller = scope.as_mut(&self.globals).fiber.take();
                    fiber.as_mut().push_call_arg_or_return_value(arg);
                    scope.as_mut(&self.globals).fiber = Some(fiber.into());
                }
                Ok(FiberAction::Try(fiber, arg)) => {
                    fiber.as_mut().caller = scope.as_mut(&self.globals).fiber.take();
                    fiber.as_mut().push_call_arg_or_return_value(arg);
                    fiber.as_mut().run_source = FiberRunSource::Try;
                    scope.as_mut(&self.globals).fiber = Some(fiber.into());
                }
                Ok(FiberAction::Suspend) => {
                    scope.as_mut(&self.globals).fiber = None;
                    // FIXME: This return value is wrong.
                    // The api should not return a value for Fiber.suspend.
                    return Ok(scope.create_null());
                }
                Ok(FiberAction::Return(value)) => {
                    let caller = if let Some(fiber) = self.fiber(scope) {
                        fiber.borrow_mut().return_from_fiber_take_caller(scope)
                    } else {
                        // This should never be reached?
                        unimplemented!();
                    };
                    scope.as_mut(&self.globals).fiber = caller.map(|local| local.into());
                    match self.fiber(scope) {
                        Some(fiber) => fiber.borrow_mut().push_return_value(value),
                        None => return Ok(value),
                    }
                }
                Ok(FiberAction::Transfer(fiber, arg)) => {
                    fiber.as_mut().push_call_arg_or_return_value(arg);
                    scope.as_mut(&self.globals).fiber = Some(fiber.into());
                }
                Ok(FiberAction::TransferError(fiber, error)) => {
                    scope.as_mut(&self.globals).fiber = Some(fiber.into());
                    self.cascade_error(scope, error)?;
                }
                Err(error) => {
                    self.cascade_error(scope, error.as_try_return_value(&scope))?;
                }
            }
        }
    }

    fn run_in_fiber<'a>(
        &mut self,
        scope: &'a HandleScope,
        fiber_ref: LocalHandle<ObjFiber>,
    ) -> Result<FiberAction<'a>, VMError> {
        loop {
            let fiber = fiber_ref.as_ref();
            // We pull the frame off of the call_stack so that in Debug mode
            // run_frame_in_fiber is still able to borrow the call_stack to
            // walk it for the active frame and frame_count.  We could pass that
            // debug info a different way I guess?
            let frame_index = fiber.call_stack.borrow().len() - 1;
            let mut frame = fiber.call_stack.borrow_mut().pop().unwrap();
            // This is all to avoid run_fiber needing to call itself
            // recursively, or the run_fiber main loop needing to pull
            // the frame on every instruction.  Maybe not worth it?
            let result = self.run_frame_in_fiber(scope, &mut frame, fiber, frame_index);
            match result {
                Ok(FunctionNext::Call(closure, num_args)) => {
                    // Push the closure and the args on the stack?
                    let mut call_stack = fiber.call_stack.borrow_mut();
                    // call_stack does not contain "frame", restore it.
                    call_stack.push(frame);
                    // Now push our new frame!
                    call_stack.push(CallFrame {
                        pc: 0,
                        closure: closure.into(),
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
                    fiber.close_upvalues_at_or_above(&scope, location);
                    // pop the frame again after upvalues are closed.
                    let frame = fiber.call_stack.borrow_mut().pop().unwrap();
                    fiber.stack.borrow_mut().truncate(frame.stack_start);

                    if fiber.call_stack.borrow().is_empty() {
                        return Ok(FiberAction::Return(value));
                    } else {
                        // Push the return value onto the calling stack.
                        fiber.push(&value.into());
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

                            let upvalue =
                                find_or_create_upvalue(&scope, fiber_ref.clone(), location);
                            closure.as_mut().push_upvalue(&upvalue.into());
                        } else {
                            // Use the same upvalue as the current call frame.
                            let stack = fiber.call_stack.borrow();
                            let top_frame = stack.last();
                            let frame = top_frame.as_ref().unwrap();
                            let upvalue = frame.closure.borrow().upvalue(compiler_upvalue.index);
                            closure.borrow_mut().push_upvalue(upvalue);
                        }
                    }
                }
                Ok(FunctionNext::CloseUpvalues(location)) => {
                    // call_stack does not contain "frame", restore it.
                    fiber.call_stack.borrow_mut().push(frame);
                    // Fiber is already borrowed, so we can't borrow_mut here,
                    // thus OpenUpvalues is independently borrowed inside
                    fiber.close_upvalues_at_or_above(&scope, location);
                    // Now actualy do the pop.
                    fiber.pop(&scope)?;
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

    // fn create_foreign<'a>(&mut self, &HandleScope, class_handle: &LocalHandle<ObjClass>) -> Result<LocalHandle<'a, ()>> {
    //     // wren_c makes these all asserts, but I'm not sure why?
    //     fn vm_assert(condition: bool, msg: &str) -> Result<()> {
    //         if condition {
    //             Ok(())
    //         } else {
    //             Err(VMError::from_str(msg))
    //         }
    //     }

    //     let class = class_handle.borrow();
    //     vm_assert(class.is_foreign(), "Class must be a foreign class.")?;

    //     // wren_c TODO: Don't look up every time.
    //     let symbol = self
    //         .methods
    //         .symbol_for_name("<allocate>")
    //         .ok_or_else(|| VMError::from_str("Should have defined <allocate> symbol."))?;

    //     vm_assert(class.methods.len() > symbol, "Class should have allocator.")?;
    //     let method = class
    //         .lookup_method(symbol)
    //         .ok_or_else(|| VMError::from_str("Class should have allocator."))?;

    //     // Pass the constructor arguments to the allocator as well.
    //     match method {
    //         Method::ForeignFunction(foreign) => {
    //             let args = vec![Value::Class(class_handle.clone())];
    //             self.call_foreign(foreign, args)
    //         }
    //         _ => Err(VMError::from_str("Allocator should be foreign.")),
    //     }
    // }

    // ~80% of benchmark time is spent under this function.
    fn call_method<'a>(
        &mut self,
        scope: &'a HandleScope,
        fiber: &ObjFiber,
        _frame: &mut CallFrame,
        num_args: usize,
        symbol: Symbol,
        class: &ObjClass,
    ) -> Result<FunctionNext<'a>, VMError> {
        // Get the Method record for this class for this symbol.
        let method = class
            .lookup_method(symbol)
            .ok_or_else(|| self.method_not_found(&class, symbol))?;

        let this_offset = fiber.stack.borrow().len() - num_args;

        // Even if we got a Method doesn't mean *this* class implements it.
        let result = match method {
            Method::ValuePrimitive(f) => {
                let value = f(scope, self, &fiber.stack.borrow()[this_offset..])?;
                fiber.stack.borrow_mut().truncate(this_offset);
                FunctionNext::Return(value)
            }
            Method::FiberActionPrimitive(f) => {
                let action = f(scope, self, &fiber.stack.borrow()[this_offset..])?;
                fiber.stack.borrow_mut().truncate(this_offset);
                FunctionNext::FiberAction(action)
            }
            Method::FunctionCall => {
                let args = &fiber.stack.borrow()[this_offset..];
                // Pushes a new stack frame.
                let closure = check_arity(&scope, &args[0], args.len())?;
                FunctionNext::Call(closure, num_args)
            }
            Method::ForeignFunction(foreign) => {
                // call_foriegn would copy the args anyway, so copy them here.
                // let value =
                //     self.call_foreign(foreign, fiber.stack.borrow_mut().split_off(this_offset))?;
                // FunctionNext::Return(value)
                unimplemented!()
            }
            Method::Block(closure) => FunctionNext::Call(scope.from_heap(closure), num_args),
        };
        Ok(result)
    }

    fn run_frame_in_fiber<'a>(
        &mut self,
        scope: &'a HandleScope,
        frame: &mut CallFrame,
        fiber: &ObjFiber,
        frame_index: usize, // for closures
    ) -> Result<FunctionNext<'a>> {
        // Copy out a ref, so we can later mut borrow the frame.
        // FIXME: Cloning every op *cannot* be correct!
        let closure_rc = scope.from_heap(&frame.closure);
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
                        module_name_and_line(&top_frame.closure.as_ref().fn_obj.borrow(), frame.pc)
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
                    fiber.push(fn_obj.lookup_constant(index));
                }
                Ops::Boolean(value) => {
                    fiber.push(&scope.create_bool(*value).erase_type().into());
                }
                Ops::Null => {
                    fiber.push(&scope.create_null().into());
                }
                Ops::CallSuper(arity, symbol, constant) => {
                    // +1 for implicit arg for 'this'.
                    let num_args = arity.as_index() + 1;
                    // Compiler error if this is not a class.
                    let superclass: LocalHandle<ObjClass> = scope
                        .from_heap(fn_obj.lookup_constant(constant))
                        .try_downcast()
                        .unwrap();
                    let action = self.call_method(
                        &scope,
                        fiber,
                        frame,
                        num_args,
                        *symbol,
                        superclass.borrow(),
                    )?;
                    match action {
                        FunctionNext::Return(value) => {
                            // We got an immediate answer from a primitive or
                            // foreign call, we can handle it here and continue.
                            fiber.push(&value.into());
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
                    let this_class =
                        self.class_for_value(&scope, &fiber.stack.borrow()[this_offset]);
                    let action = self.call_method(
                        &scope,
                        fiber,
                        frame,
                        num_args,
                        *symbol,
                        &this_class.borrow(),
                    )?;
                    match action {
                        FunctionNext::Return(value) => {
                            // We got an immediate answer from a primitive or
                            // foreign call, we can handle it here and continue.
                            fiber.push(&value.into());
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
                    let this = fiber.load_this(scope, frame);
                    let class = this.try_downcast().expect("'this' should be a class.");
                    let instance = ObjInstance::new(&scope, class);
                    fiber.store_this(frame, instance.erase_type());
                }
                Ops::ForeignConstruct => {
                    unimplemented!("ForeignConstruct");
                    // let this = fiber.load_this(frame);
                    // let class = this.try_into_class().expect("'this' should be a class.");
                    // let instance = self.create_foreign(&class)?;
                    // fiber.store_this(frame, instance);
                }
                Ops::Closure(constant, upvalues) => {
                    let fn_value = fn_obj.lookup_constant(constant).clone();
                    let fn_obj = scope
                        .from_heap(&fn_value)
                        .try_downcast()
                        .ok_or_else(|| VMError::from_str("constant was not closure"))?;
                    let closure = ObjClosure::new(self, &scope, fn_obj);
                    fiber.push(&closure.erase_type().into());
                    if !upvalues.is_empty() {
                        return Ok(FunctionNext::CaptureUpvalues(closure, upvalues.to_vec()));
                    }
                    // Optimization: if no upvalues, continue as normal.
                }
                Ops::EndModule => {
                    scope.as_mut(&self.globals).last_imported_module = Some(fn_obj.module.clone());
                    fiber.push(&scope.create_null().erase_type().into()); // Is this the return value?
                }
                Ops::Return => {
                    // The top of our stack is returned and then the caller of
                    // this rust function will push the return onto the stack of
                    // the calling wren function CallFrame.
                    return Ok(FunctionNext::Return(fiber.pop(&scope)?));
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
                        &scope,
                        fiber,
                        ClassSource::Source(*num_fields),
                        &fn_obj.module.borrow(),
                    )?;
                }
                Ops::ForeignClass => {
                    create_class(
                        self,
                        &scope,
                        fiber,
                        ClassSource::Foreign,
                        &fn_obj.module.borrow(),
                    )?;
                }
                Ops::Load(variable) => {
                    let value = match *variable {
                        Variable::Module(index) => {
                            scope.from_heap(&fn_obj.module.borrow().variables[index])
                        }
                        Variable::Upvalue(index) => closure.upvalue(index).borrow().load(&scope),
                        Variable::Local(index) => fiber.load_local(scope, frame, index),
                    };
                    fiber.push(&value.into());
                }
                Ops::Store(variable) => {
                    let value = fiber.peek(scope)?;
                    match *variable {
                        Variable::Module(index) => {
                            fn_obj.module.borrow_mut().variables[index] = value.into()
                        }
                        Variable::Upvalue(index) => {
                            closure.upvalue(index).borrow_mut().store(&value.into())
                        }
                        Variable::Local(index) => fiber.store_local(frame, index, &value.into()),
                    };
                }
                Ops::LoadField(symbol) => {
                    let receiver = fiber.pop(&scope)?;
                    let instance: LocalHandle<ObjInstance> = receiver
                        .try_downcast()
                        .ok_or_else(|| VMError::from_str("Receiver should be instance."))?;
                    if *symbol >= instance.borrow().fields.len() {
                        return Err(VMError::from_str("Out of bounds field."));
                    }
                    fiber.push(&instance.borrow().fields[*symbol]);
                }
                Ops::StoreField(symbol) => {
                    let receiver = fiber.pop(&scope)?;
                    let instance: LocalHandle<ObjInstance> = receiver
                        .try_downcast()
                        .ok_or_else(|| VMError::from_str("Receiver should be instance."))?;
                    if *symbol >= instance.borrow().fields.len() {
                        return Err(VMError::from_str("Out of bounds field."));
                    }
                    instance.borrow_mut().fields[*symbol] = fiber.peek(scope)?.into();
                }
                Ops::Pop => {
                    fiber.pop(&scope)?;
                }
                Ops::Method(is_static, symbol) => {
                    // wren_c peeks first then drops after bind, unclear why
                    let class: LocalHandle<ObjClass> = fiber.pop(&scope)?.try_downcast().unwrap();
                    let method = fiber.pop(&scope)?;
                    bind_method(
                        self,
                        &scope,
                        *is_static,
                        *symbol,
                        &fn_obj.module.borrow(),
                        class.borrow_mut(),
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
                    let result =
                        import_module(self, scope, &fn_obj.module.borrow().name, module_name)?;
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
                            fiber.push(
                                &scope
                                    .take("dummy for module".to_string())
                                    .unwrap()
                                    .erase_type()
                                    .into(),
                            );
                            scope.as_mut(&self.globals).last_imported_module = Some(module.into());
                        }
                    }
                }
                Ops::ImportVariable(variable_name) => {
                    let module = scope
                        .as_ref(&self.globals)
                        .last_imported_module
                        .as_ref()
                        .expect("Should have already imported module.");
                    let value = get_module_variable(&scope, module.as_ref(), variable_name)?;
                    fiber.push(&value.into());
                }
                Ops::Loop(offset_backwards) => {
                    frame.pc -= *offset_backwards as usize;
                }
                Ops::Jump(offset_forward) => {
                    frame.pc += *offset_forward as usize;
                }
                Ops::JumpIfFalse(offset_forward) => {
                    let value = fiber.pop(&scope)?;
                    if is_falsey(&value.into()) {
                        frame.pc += *offset_forward as usize;
                    }
                }
                Ops::And(offset_forward) => {
                    // This differs from JumpIfFalse in whether it pops
                    let value = fiber.peek(scope)?;
                    if is_falsey(&value.into()) {
                        frame.pc += *offset_forward as usize;
                    } else {
                        fiber.pop(&scope)?;
                    }
                }
                Ops::Or(offset_forward) => {
                    let value = fiber.peek(scope)?;
                    if !is_falsey(&value.into()) {
                        frame.pc += *offset_forward as usize;
                    } else {
                        fiber.pop(&scope)?;
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

fn check_arity<'a>(
    scope: &'a HandleScope,
    value: &HeapHandle<()>,
    num_args: usize,
) -> Result<LocalHandle<'a, ObjClosure>> {
    let closure: LocalHandle<ObjClosure> = scope
        .from_heap(value)
        .try_downcast()
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
    stack: Option<&[HeapHandle<()>]>,
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
    stack: Option<&[HeapHandle<()>]>,
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
                Some(c) => format!("Load(Upvalue, {})", index),
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

pub(crate) fn debug_bytecode(scope: &HandleScope, vm: &VM, top_closure: &ObjClosure) {
    let mut fn_objs = vec![scope.from_heap::<ObjFn>(&top_closure.fn_obj)];

    loop {
        let fn_obj = fn_objs.remove(0);
        let func = fn_obj.as_ref();
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
            let maybe_fn: Option<LocalHandle<ObjFn>> = scope.from_heap(const_value).try_downcast();
            if let Some(fn_obj) = maybe_fn {
                fn_objs.push(fn_obj.into());
            }
        }
        // Walk functions/closures looking for code?

        if fn_objs.is_empty() {
            break;
        }
    }
}

pub trait Obj: HostObject {
    // The object's class.
    fn class_obj(&self) -> &HeapHandle<ObjClass>;
}

pub(crate) struct ObjRange {
    class_obj: HeapHandle<ObjClass>,
    // The beginning of the range.
    pub(crate) from: f64,
    // The end of the range. May be greater or less than [from].
    pub(crate) to: f64,
    // True if [to] is included in the range.
    pub(crate) is_inclusive: bool,
}

impl Traceable for ObjRange {
    fn trace(&mut self, visitor: &mut ObjectVisitor) {
        self.class_obj.trace(visitor);
    }
}

impl ObjRange {
    pub(crate) fn min(&self) -> f64 {
        self.from.min(self.to)
    }

    pub(crate) fn max(&self) -> f64 {
        self.from.max(self.to)
    }
}

pub(crate) struct ObjMap {
    class_obj: HeapHandle<ObjClass>,
    pub(crate) data: Map<(), ()>,
}

impl Traceable for ObjMap {
    fn trace(&mut self, visitor: &mut ObjectVisitor) {
        self.class_obj.trace(visitor);
        self.data.trace(visitor);
    }
}

impl ObjMap {
    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn contains_key(&self, key: LocalHandle<()>) -> bool {
        self.data.contains_key(&key.into())
    }
}

impl HostObject for ObjMap {
    const TYPE_ID: ObjectType = ObjectType::Host;
}

impl Obj for ObjMap {
    fn class_obj(&self) -> &HeapHandle<ObjClass> {
        &self.class_obj
    }
}

pub(crate) struct ObjList {
    class_obj: HeapHandle<ObjClass>,
    pub(crate) elements: List<()>,
}

impl Traceable for ObjList {
    fn trace(&mut self, visitor: &mut ObjectVisitor) {
        self.class_obj.trace(visitor);
        self.elements.trace(visitor);
    }
}

impl ObjList {
    pub fn len(&self) -> usize {
        self.elements.len()
    }
}

impl HostObject for ObjList {
    const TYPE_ID: ObjectType = ObjectType::Host;
}

impl Obj for ObjList {
    fn class_obj(&self) -> &HeapHandle<ObjClass> {
        &self.class_obj
    }
}

pub(crate) struct ObjClosure {
    class_obj: HeapHandle<ObjClass>,
    pub(crate) fn_obj: HeapHandle<ObjFn>,
    upvalues: List<Upvalue>,
}

impl Traceable for ObjClosure {
    fn trace(&mut self, visitor: &mut ObjectVisitor) {
        self.class_obj.trace(visitor);
        self.fn_obj.trace(visitor);
        self.upvalues.trace(visitor);
    }
}

impl ObjClosure {
    pub(crate) fn new<'a>(
        vm: &VM,
        scope: &'a HandleScope,
        fn_obj: LocalHandle<ObjFn>,
    ) -> LocalHandle<'a, ObjClosure> {
        // FIXME: Is this really supposed to also be class = fn?
        scope
            .take(ObjClosure {
                class_obj: scope.as_ref(&vm.globals).fn_class.as_ref().unwrap().clone(),
                fn_obj: fn_obj.into(),
                upvalues: List::default(),
            })
            .unwrap()
    }

    fn push_upvalue(&mut self, upvalue: &HeapHandle<Upvalue>) {
        self.upvalues.push(upvalue.clone())
    }

    fn upvalue(&self, index: usize) -> &HeapHandle<Upvalue> {
        &self.upvalues[index]
    }
}

impl HostObject for ObjClosure {
    const TYPE_ID: ObjectType = ObjectType::Host;
}

impl Obj for ObjClosure {
    fn class_obj(&self) -> &HeapHandle<ObjClass> {
        &self.class_obj
    }
}

pub(crate) struct ObjFn {
    class_obj: HeapHandle<ObjClass>,
    pub(crate) arity: Arity,
    pub(crate) constants: List<()>,
    pub(crate) code: Vec<Ops>,
    pub(crate) debug: FnDebug,
    // This is needed so that we can find the Module through which to
    // do module-level loads/stores when executing.  This is *definitely*
    // circular reference.
    pub(crate) module: HeapHandle<Module>,
}

impl Traceable for ObjFn {
    fn trace(&mut self, visitor: &mut ObjectVisitor) {
        self.class_obj.trace(visitor);
        self.constants.trace(visitor);
        // FIXME: Do we need to trace self.code?
        self.module.trace(visitor);
    }
}

fn count_params_in_signature(signature: &str) -> usize {
    let mut num_params = 0;
    let signature_length = signature.len();
    // Count normal arguments
    if signature.ends_with(')') {
        let left_paren_index = signature.rfind('(').unwrap();
        let params = &signature[(left_paren_index + 1)..signature_length];
        num_params += params.matches('_').count();
    }
    // Count subscript arguments.
    if signature.starts_with('[') {
        let right_brace_index = signature.find(']').unwrap();
        let params = &signature[1..right_brace_index];
        num_params += params.matches('_').count();
    }
    num_params
}

impl ObjFn {
    pub(crate) fn from_compiler<'a>(
        vm: &VM,
        scope: &'a HandleScope,
        module: &GlobalHandle<Module>,
        compiler: crate::compiler::Compiler,
        arity: Arity,
    ) -> LocalHandle<'a, ObjFn> {
        scope
            .take(ObjFn {
                class_obj: scope.as_ref(&vm.globals).fn_class.as_ref().unwrap().clone(),
                constants: scope.as_ref(&compiler.constants).list.clone(),
                code: compiler.code,
                arity,
                debug: compiler.fn_debug,
                module: scope.from_global(module).into(),
            })
            .unwrap()
    }

    // pub(crate) fn set_constant(&mut self, constant: &Constant, value: Value) {
    //     self.constants[constant.as_index()] = value;
    // }

    pub(crate) fn lookup_constant(&self, constant: &Constant) -> &HeapHandle<()> {
        &self.constants[constant.as_index()]
    }

    pub(crate) fn stub_call<'a>(
        vm: &VM,
        scope: &'a HandleScope,
        signature: &str,
        symbol: Symbol,
    ) -> LocalHandle<'a, ObjFn> {
        let num_params_usize = count_params_in_signature(signature);
        let num_params: u8 = u8::try_from(num_params_usize).unwrap();
        let code = vec![Ops::Call(Arity(num_params), symbol), Ops::Return, Ops::End];
        let code_len = code.len();

        scope
            .take(ObjFn {
                class_obj: scope.as_ref(&vm.globals).fn_class.as_ref().unwrap().clone(),
                // FIXME: Why is this Arity + 1 and the above is not?
                arity: Arity(num_params + 1),
                code: code,
                constants: List::default(),
                module: scope
                    .as_ref(&vm.globals)
                    .last_imported_module
                    .as_ref()
                    .unwrap()
                    .clone(),
                debug: FnDebug::generated(signature, code_len),
            })
            .unwrap()
    }
}

impl HostObject for ObjFn {
    const TYPE_ID: ObjectType = ObjectType::Host;
}

impl Obj for ObjFn {
    fn class_obj(&self) -> &HeapHandle<ObjClass> {
        &self.class_obj
    }
}

pub(crate) struct ObjInstance {
    class_obj: HeapHandle<ObjClass>,
    fields: List<()>,
}

impl Traceable for ObjInstance {
    fn trace(&mut self, visitor: &mut ObjectVisitor) {
        self.class_obj.trace(visitor);
        self.fields.trace(visitor);
    }
}

impl ObjInstance {
    pub(crate) fn new<'a>(
        scope: &'a HandleScope,
        class: LocalHandle<ObjClass>,
    ) -> LocalHandle<'a, ObjInstance> {
        let num_fields = class
            .borrow()
            .num_fields()
            .expect("Compiler emitted Construct for non-source class.");
        // FIXME: Need a vec!-like constructor for List?
        let fields = vec![scope.create_null(); num_fields];
        scope
            .take(ObjInstance {
                class_obj: class.into(),
                fields: List::from(fields),
            })
            .unwrap()
    }
}

impl HostObject for ObjInstance {
    const TYPE_ID: ObjectType = ObjectType::Host;
}

impl Obj for ObjInstance {
    fn class_obj(&self) -> &HeapHandle<ObjClass> {
        &self.class_obj
    }
}

pub(crate) struct ObjForeign {
    class_obj: HeapHandle<ObjClass>,
    pub user_data: Box<dyn UserData>,
}

impl Traceable for ObjForeign {
    fn trace(&mut self, visitor: &mut ObjectVisitor) {
        self.class_obj.trace(visitor);
    }
}

impl ObjForeign {
    pub(crate) fn new<'a>(
        class: LocalHandle<ObjClass>,
        scope: &'a HandleScope,
        user_data: Box<dyn UserData>,
    ) -> LocalHandle<'a, ObjForeign> {
        scope
            .take(ObjForeign {
                class_obj: class.into(),
                user_data,
            })
            .unwrap()
    }
}

impl HostObject for ObjForeign {
    const TYPE_ID: ObjectType = ObjectType::Host;
}

impl Obj for ObjForeign {
    fn class_obj(&self) -> &HeapHandle<ObjClass> {
        &self.class_obj
    }
}

impl core::fmt::Debug for ObjRange {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let op_string = if self.is_inclusive { ".." } else { "..." };
        write!(f, "{}{}{}", self.from, op_string, self.to)
    }
}

impl HostObject for ObjRange {
    const TYPE_ID: ObjectType = ObjectType::Host;
}

impl Obj for ObjRange {
    fn class_obj(&self) -> &HeapHandle<ObjClass> {
        &self.class_obj
    }
}

// impl core::fmt::Debug for dyn Obj {
//     fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
//         write!(f, "Object{:?}", self.obj_type())
//     }
// }

pub(crate) enum FiberAction<'a> {
    Call(LocalHandle<'a, ObjFiber>, LocalHandle<'a, ()>),
    Transfer(LocalHandle<'a, ObjFiber>, LocalHandle<'a, ()>),
    TransferError(LocalHandle<'a, ObjFiber>, LocalHandle<'a, ()>),
    // AFAICT, Return and Yield are the same.
    Return(LocalHandle<'a, ()>),
    Try(LocalHandle<'a, ObjFiber>, LocalHandle<'a, ()>),
    Suspend,
}

type ValuePrimitive = for<'a> fn(
    scope: &'a HandleScope,
    vm: &VM,
    args: &[HeapHandle<()>],
) -> Result<LocalHandle<'a, ()>>;
type FiberActionPrimitive =
    for<'a> fn(scope: &'a HandleScope, vm: &VM, args: &[HeapHandle<()>]) -> Result<FiberAction<'a>>;

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
    Block(HeapHandle<ObjClosure>),
}

impl Traceable for Method {
    fn trace(&mut self, visitor: &mut ObjectVisitor) {
        match self {
            Self::Block(handle) => handle.trace(visitor),
            Self::ValuePrimitive(_) => {}
            Self::FiberActionPrimitive(_) => {}
            Self::FunctionCall => {}
            Self::ForeignFunction(_) => {}
        }
    }
}

impl HostObject for Method {
    const TYPE_ID: ObjectType = ObjectType::Host;
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

// Derive from Default to allow take() to work during clear().
#[derive(Default)]
pub struct ObjClass {
    // Class can be null during Object and Class and Object metaclass
    // initialization starting with class = null and then filling in.
    pub(crate) class: Option<HeapHandle<ObjClass>>,

    // Class is the only class w/o a superclass, all others this is Some(class).
    pub(crate) superclass: Option<HeapHandle<ObjClass>>,

    // What created this class.  Including the number of fields it has if it
    // was created from source, rather than internal or foreign.
    source: ClassSource,

    // The table of methods that are defined in or inherited by this class.
    // Methods are called by symbol, and the symbol directly maps to an index in
    // this table. This makes method calls fast at the expense of empty cells in
    // the list for methods the class doesn't support.
    //
    // You can think of it as a hash table that never has collisions but has a
    // really low load factor. Since methods are pretty small (just a type and a
    // pointer), this should be a worthwhile trade-off.
    methods: Vec<Option<Method>>,

    // The name of the class.
    pub(crate) name: String,
    // The ClassAttribute for the class, if any
    //   Value attributes;
}

impl Traceable for ObjClass {
    fn trace(&mut self, visitor: &mut ObjectVisitor) {
        // FIXME: Implement Option<Traceable>::trace instead?
        if let Some(class) = &self.class {
            class.trace(visitor);
        }
        if let Some(superclass) = &self.superclass {
            superclass.trace(visitor);
        }
        for maybe_method in self.methods.iter_mut() {
            if let Some(method) = maybe_method {
                method.trace(visitor);
            }
        }
    }
}

impl ObjClass {
    fn num_fields(&self) -> Option<usize> {
        match self.source {
            ClassSource::Source(num_fields) => Some(num_fields),
            _ => None, // Not a valid question to ask.
        }
    }

    fn is_foreign(&self) -> bool {
        matches!(self.source, ClassSource::Foreign)
    }
}

impl core::fmt::Debug for ObjClass {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "ObjClass {{ {} ", self.name)?;
        match &self.superclass {
            None => write!(f, "super: None ")?,
            // FIXME: This is another instance where having an iterator
            // over the class hierarchy could be useful to print
            // Bool->Object, etc.
            Some(rc) => write!(f, "super: {:?} ", rc.borrow().name)?,
        }
        match &self.class {
            None => write!(f, "meta: None ")?,
            Some(rc) => write!(f, "meta: {:?} ", rc.borrow().name)?,
        }
        write!(f, "}}")
    }
}

impl ObjClass {
    // wren_c calls this wrenBindMethod
    pub(crate) fn set_method(&mut self, symbol: Symbol, method: Method) {
        if symbol >= self.methods.len() {
            self.methods.resize(symbol + 1, None);
        }
        self.methods[symbol] = Some(method);
    }

    fn inherit_methods_from(&mut self, superclass: &ObjClass) {
        // Should this assert that we're not overriding any methods?
        self.methods
            .resize(self.methods.len() + superclass.methods.len(), None);
        for (symbol, method) in superclass.methods.iter().enumerate() {
            self.methods[symbol] = method.clone();
        }
    }

    pub(crate) fn lookup_method(&self, symbol: Symbol) -> Option<&Method> {
        match self.methods.get(symbol) {
            Some(maybe_method) => maybe_method.as_ref(),
            None => None,
        }
    }
}

impl HostObject for ObjClass {
    const TYPE_ID: ObjectType = ObjectType::Host;
}

impl Obj for ObjClass {
    fn class_obj(&self) -> &HeapHandle<ObjClass> {
        &self.class.as_ref().unwrap()
    }
}

#[cfg(test)]
mod tests {
    // #[test]
    // fn value_num_hash_unique() {
    //     fn hash_as_value(num: f64) -> u64 {
    //         use std::hash::{Hash, Hasher};
    //         let value = super::Value::Num(num);
    //         let mut s = std::collections::hash_map::DefaultHasher::new();
    //         value.hash(&mut s);
    //         s.finish()
    //     }
    //     assert_ne!(hash_as_value(1.0), hash_as_value(2.0));
    //     assert_eq!(hash_as_value(1.0), hash_as_value(1.0));
    //     assert_eq!(hash_as_value(3.0), hash_as_value(1.0 + 2.0));
    //     // Floats equality is hard.  We don't try to fix that:
    //     assert_ne!(hash_as_value(0.3), hash_as_value(0.1 + 0.2));
    // }
}
