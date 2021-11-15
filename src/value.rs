// use crate::vm::{CallFrame, Clear, FiberRunSource, OpenUpvalues, StackOffset, VM};
use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

use crate::compiler::{Arity, Constant, FnDebug, Ops};
use crate::vm::*;

type Handle<T> = Rc<RefCell<T>>;
type Result<T, E = VMError> = std::result::Result<T, E>;

#[derive(Clone)]
pub(crate) enum Value {
    Null,
    Num(f64),
    Boolean(bool),
    String(Rc<String>),
    // Split these off and replace with Object(Handle<dyn Obj>)
    Class(Handle<ObjClass>),
    Range(Handle<ObjRange>),
    Fn(Handle<ObjFn>),
    Closure(Handle<ObjClosure>),
    List(Handle<ObjList>),
    Map(Handle<ObjMap>),
    Fiber(Handle<ObjFiber>),
    Instance(Handle<ObjInstance>),
    Foreign(Handle<ObjForeign>),
}

impl Hash for Value {
    // See hashObject in wren_c wren_value.c
    fn hash<H: Hasher>(&self, state: &mut H) {
        fn hash_f64<H: Hasher>(num: f64, state: &mut H) {
            let bits: u64 = num.to_bits();
            bits.hash(state);
        }
        match self {
            Value::Null => 0.hash(state),
            Value::Num(f64_ref) => hash_f64(*f64_ref, state),
            Value::Boolean(v) => v.hash(state),
            Value::String(v) => v.hash(state),
            Value::Class(v) => v.borrow().name.hash(state),
            Value::Range(v) => {
                hash_f64(v.borrow().from, state);
                hash_f64(v.borrow().to, state);
                // wren_c doesn't hash inclusive, but we could?
            }
            // FIXME: wren_c defines a hash for Fn for internal usage?
            Value::Fn(v) => v.as_ptr().hash(state),
            // FIXME: The rest of these may be wrong?
            Value::Closure(v) => v.as_ptr().hash(state),
            Value::List(v) => v.as_ptr().hash(state),
            Value::Map(v) => v.as_ptr().hash(state),
            Value::Fiber(v) => v.as_ptr().hash(state),
            Value::Instance(v) => v.as_ptr().hash(state),
            Value::Foreign(v) => v.as_ptr().hash(state),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, rhs: &Value) -> bool {
        // wren_c does memcmp here (e.g. wrenValuesEqual)
        // However the code below seems to pass all the tests.
        match (self, rhs) {
            (Value::Null, Value::Null) => true,
            (Value::Num(a), Value::Num(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Range(a_range), Value::Range(b_range)) => {
                let a = a_range.borrow();
                let b = b_range.borrow();
                a.from == b.from && a.to == b.to && a.is_inclusive == b.is_inclusive
            }
            (Value::String(a_string), Value::String(b_string)) => a_string.eq(&b_string),
            (Value::Class(a), Value::Class(b)) => a.as_ptr() == b.as_ptr(),
            (Value::Instance(a), Value::Instance(b)) => a.as_ptr() == b.as_ptr(),
            (Value::Fn(a), Value::Fn(b)) => a.as_ptr() == b.as_ptr(),
            (Value::List(a), Value::List(b)) => a.as_ptr() == b.as_ptr(),
            (Value::Map(a), Value::Map(b)) => a.as_ptr() == b.as_ptr(),
            (Value::Closure(a), Value::Closure(b)) => a.as_ptr() == b.as_ptr(),
            (Value::Fiber(a), Value::Fiber(b)) => a.as_ptr() == b.as_ptr(),
            // FIXME: This catch-all is kinda dangerous as it prevents
            // warnings when we add new types to Value.
            // Some way to only catch-all for (T, S) where T != S
            // would be more future-proof.
            _ => false,
        }
    }
}

impl Eq for Value {}

impl core::fmt::Debug for Value {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Value::Null => write!(f, "Null"),
            Value::Num(n) => write!(f, "{}", n),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::Class(c) => write!(f, "Class(\"{}\")", c.borrow().name),
            Value::Range(r) => write!(f, "{:?}", r.borrow()),
            Value::List(l) => write!(f, "List(len: {})", l.borrow().len()),
            Value::Map(m) => write!(f, "Map(len: {})", m.borrow().len()),
            Value::Fiber(_) => write!(f, "Fiber()"),
            Value::Fn(c) => write!(f, "Fn({})", c.borrow().debug.name),
            Value::Closure(c) => write!(f, "{}", c.borrow().fn_obj.borrow().debug.name),
            Value::Instance(o) => write!(
                f,
                "Instance({})",
                // FIXME: Probably needs a helper on ObjInstance?
                o.borrow().class_obj().borrow().name
            ),
            Value::Foreign(o) => write!(f, "Foreign({})", o.borrow().class_obj().borrow().name),
        }
    }
}

impl Value {
    pub(crate) fn from_string(string: String) -> Value {
        Value::String(Rc::new(string))
    }

    pub(crate) fn from_str(string: &str) -> Value {
        Self::from_string(string.into())
    }

    // Make num casts explicit and easily searchable:
    pub(crate) fn from_usize(num: usize) -> Value {
        Self::Num(num as f64)
    }
    pub(crate) fn from_u8(num: u8) -> Value {
        Self::Num(num as f64)
    }
    pub(crate) fn from_u32(num: u32) -> Value {
        Self::Num(num as f64)
    }

    // Different from "is truthy" used by "as_bool" in wren_c
    // Unclear if this is only ever called on a known-bool?
    pub(crate) fn equals_true(&self) -> bool {
        matches!(self, Value::Boolean(true))
    }

    // In Wren false and null are false, everything else is true.
    fn is_truthy(&self) -> bool {
        match self {
            Value::Null => false,
            Value::Boolean(b) => *b,
            _ => true,
        }
    }

    pub fn is_falsey(&self) -> bool {
        !self.is_truthy()
    }

    pub(crate) fn is_null(&self) -> bool {
        matches!(self, Value::Null)
    }
    pub(crate) fn is_num(&self) -> bool {
        matches!(self, Value::Num(_))
    }
}

macro_rules! try_into {
    ($func:ident,  $value_type:ident, $return_type:ident) => {
        pub fn $func(&self) -> Option<Handle<$return_type>> {
            match self {
                Value::$value_type(value) => Some(value.clone()),
                _ => None,
            }
        }
    };
}

impl Value {
    pub fn try_into_num(&self) -> Option<f64> {
        match self {
            Value::Num(value) => Some(*value),
            _ => None,
        }
    }

    pub fn try_into_bool(&self) -> Option<bool> {
        match self {
            Value::Boolean(value) => Some(*value),
            _ => None,
        }
    }

    pub fn try_into_string(&self) -> Option<String> {
        match self {
            Value::String(string) => Some(string.as_ref().clone()),
            _ => None,
        }
    }

    try_into!(try_into_range, Range, ObjRange);
    try_into!(try_into_map, Map, ObjMap);
    try_into!(try_into_class, Class, ObjClass);
    try_into!(try_into_list, List, ObjList);
    try_into!(try_into_fn, Fn, ObjFn);
    try_into!(try_into_closure, Closure, ObjClosure);
    try_into!(try_into_instance, Instance, ObjInstance);
    try_into!(try_into_fiber, Fiber, ObjFiber);
    try_into!(try_into_foreign, Foreign, ObjForeign);
}

struct OpenUpvalues {
    values: Vec<Handle<Upvalue>>,
}

impl OpenUpvalues {
    fn find_open_upvalue(&self, location: &StackOffset) -> Option<Handle<Upvalue>> {
        for upvalue in &self.values {
            if upvalue.borrow().has_location(location) {
                return Some(upvalue.clone());
            }
        }
        None
    }

    fn push(&mut self, upvalue: Handle<Upvalue>) {
        assert!(upvalue.borrow().is_open());
        self.values.push(upvalue);
    }
}

pub struct ObjFiber {
    class_obj: Handle<ObjClass>,
    pub(crate) error: Value,
    pub(crate) caller: Option<Handle<ObjFiber>>,
    // We can't grab at the call_stack to check if empty
    // while it might be held mutably for execution, so we cache
    // the "completed_normally" bool here.
    // FIXME: This is probably better as an enum?
    pub(crate) completed_normally_cache: bool,
    pub run_source: FiberRunSource,
    pub(crate) stack: RefCell<Vec<Value>>,
    // Hels in a RefCell so others can interact with the rest of
    // ObjFiber (to ask class, etc.) while the stack is  held mutably
    // for the executing fiber.
    pub call_stack: RefCell<Vec<CallFrame>>,
    open_upvalues: RefCell<OpenUpvalues>,
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

    pub fn return_from_fiber_take_caller(&mut self) -> Option<Handle<ObjFiber>> {
        let caller = self.caller.take();
        self.completed_normally_cache = self.call_stack.borrow().is_empty();
        caller
    }

    // Can only be 0 or 1, enforced by Fiber.new.
    // Only valid to call when not running.
    fn arity(&self) -> u8 {
        self.call_stack.borrow()[0]
            .closure
            .borrow()
            .fn_obj
            .borrow()
            .arity
            .as_u8()
    }

    pub(crate) fn push_call_arg_or_return_value(&mut self, arg: Value) {
        if self.just_started() {
            // The fiber is being started for the first time. If its
            // function takes a parameter, bind an argument to it.
            // This exact check of arity == 1 is OK, because
            // Fiber.new also checks arity is either 0 or 1.
            if self.arity() == 1 {
                self.push(arg);
            }
        } else {
            // The fiber is being resumed, make yield(), transfer()
            // or transferError() return the result.
            self.push(arg);
        }
    }

    pub(crate) fn push_return_value(&mut self, arg: Value) {
        // Push the argument to the fiber call, try or transfer
        // or the return value from a yield or transfer
        // onto the top-most frame of the callstack.
        self.push(arg);
    }

    pub(crate) fn error(&self) -> Value {
        self.error.clone()
    }

    pub fn has_error(&self) -> bool {
        !self.error.is_null()
    }

    fn index_for_stack_offset(&self, offset: StackOffset) -> usize {
        self.call_stack.borrow()[offset.frame_index].stack_start + offset.index
    }

    pub(crate) fn load(&self, loc: StackOffset) -> Value {
        let index = self.index_for_stack_offset(loc);
        self.stack.borrow()[index].clone()
    }

    pub(crate) fn store(&self, loc: StackOffset, value: Value) {
        let index = self.index_for_stack_offset(loc);
        self.stack.borrow_mut()[index] = value
    }

    // FIXME: Should this take a Frame for bounds checking?
    #[inline] // 3% on map_numeric
    pub(crate) fn push(&self, value: Value) {
        self.stack.borrow_mut().push(value);
    }

    // FIXME: No longer needs to be Result, now that Stack Underflow panics
    pub(crate) fn pop(&self) -> Result<Value> {
        Ok(self.stack.borrow_mut().pop().expect("Stack underflow"))
    }

    // FIXME: No longer needs to be Result, now that Stack Underflow panics
    // FIXME: This could return a &Value instead of Value, but the only
    // two callers who *do not* immediately clone() are And and Or ops.
    pub(crate) fn peek(&self) -> Result<Value> {
        Ok(self.stack.borrow().last().expect("Stack underflow").clone())
    }

    pub(crate) fn load_this(&self, frame: &CallFrame) -> Value {
        self.load_local(frame, 0)
    }

    pub(crate) fn store_this(&self, frame: &CallFrame, value: Value) {
        self.store_local(frame, 0, value)
    }

    pub(crate) fn load_local(&self, frame: &CallFrame, offset: usize) -> Value {
        // FIXME: bounds-check the number of locals based on compiler data?
        self.stack.borrow()[frame.stack_start + offset].clone()
    }

    pub(crate) fn store_local(&self, frame: &CallFrame, offset: usize, value: Value) {
        // FIXME: bounds-check the number of locals based on compiler data?
        self.stack.borrow_mut()[frame.stack_start + offset] = value
    }

    pub fn dump_stack(&self, frame: &CallFrame, active_module: &str, frame_depth: usize) {
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

// FIXME: Could be a static on ObjFiber?
pub(crate) fn find_or_create_upvalue(
    fiber_ref: &Handle<ObjFiber>,
    location: StackOffset,
) -> Handle<Upvalue> {
    let fiber = fiber_ref.borrow();
    // wren_c uses a Value* for the search and the knowledge that values
    // are all held on a single stack/vector.
    // If the upvalue is still open, re-use that (so that modifications
    // are reflected in all callers).
    if let Some(upvalue) = fiber.open_upvalues.borrow().find_open_upvalue(&location) {
        return upvalue.clone();
    }
    // If there isn't yet an upvalue (or there are, but they're closed but
    // running the function again, etc.), we make a new one.
    let upvalue = Upvalue::new(fiber_ref.clone(), location);
    fiber.open_upvalues.borrow_mut().push(upvalue.clone());
    upvalue
}

impl Obj for ObjFiber {
    fn class_obj(&self) -> Handle<ObjClass> {
        self.class_obj.clone()
    }
}

impl ObjFiber {
    pub(crate) fn new(
        vm: &VM,
        closure: Handle<ObjClosure>,
        run_source: FiberRunSource,
    ) -> ObjFiber {
        ObjFiber {
            class_obj: vm.fiber_class.as_ref().unwrap().clone(),
            error: Value::Null,
            caller: None,
            run_source,
            completed_normally_cache: false,
            call_stack: RefCell::new(vec![CallFrame::new(closure.clone(), 0)]),
            stack: RefCell::new(vec![Value::Closure(closure)]),
            open_upvalues: RefCell::new(OpenUpvalues { values: vec![] }),
        }
    }
}

impl core::fmt::Debug for ObjFiber {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let stack = self.call_stack.borrow();
        write!(f, "call_stack: (len {}), ", stack.len())
    }
}

impl Clear for ObjFiber {
    fn clear(&mut self) {
        self.call_stack.borrow_mut().clear();
    }
}

impl Clear for ObjClass {
    fn clear(&mut self) {
        clear_class(self.class.take());
        clear_class(self.superclass.take());
        self.methods.clear();
    }
}

impl Clear for ObjInstance {
    fn clear(&mut self) {
        self.class_obj.borrow_mut().clear();
        self.fields.clear();
    }
}

impl Clear for ObjForeign {
    fn clear(&mut self) {
        self.class_obj.borrow_mut().clear();
    }
}

impl Clear for ObjList {
    fn clear(&mut self) {
        self.elements.clear();
    }
}

impl Clear for ObjMap {
    fn clear(&mut self) {
        self.data.clear();
    }
}

impl Clear for ObjRange {
    fn clear(&mut self) {
        // No object references held.
    }
}

impl Clear for ObjFn {
    fn clear(&mut self) {
        self.class_obj.borrow_mut().clear();
        self.constants.clear();
        self.code.clear();
        self.module.borrow_mut().clear();
    }
}

impl Clear for ObjClosure {
    fn clear(&mut self) {
        self.class_obj.borrow_mut().clear();
        self.fn_obj.borrow_mut().clear();
    }
}

impl ObjClass {
    pub(crate) fn bind_superclass(&mut self, superclass: &Handle<ObjClass>) {
        self.superclass = Some(superclass.clone());

        // Include the superclass in the total number of fields.
        match &mut self.source {
            ClassSource::Source(num_fields) => {
                *num_fields += superclass.borrow().num_fields().unwrap_or(0);
            }
            _ => match superclass.borrow().num_fields() {
                Some(num_fields) if num_fields > 0 => {
                    panic!("A foreign class cannot inherit from a class with fields.");
                }
                _ => {}
            },
        }

        // Inherit methods from its superclass.
        // FIXME: Should this be in reverse order (to minimize # of resizes?)
        self.inherit_methods_from(&superclass.borrow());
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
    pub fn close_upvalues_at_or_above(&self, location: StackOffset) {
        let close_upvalue_if_above = |u: &mut Handle<Upvalue>| {
            // All open values have a location.
            let l = u.borrow().location().unwrap();
            if l >= location {
                // Move the value into the upvalue itself and point the upvalue to it.
                let value = self.load(l);
                u.borrow_mut().storage = UpvalueStorage::Closed(value);
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
                vec.remove(i);
            } else {
                i += 1;
            }
        }
    }
}

impl ObjClass {
    // FIXME: Does this belong on ObjClass or ObjFn or free?
    pub(crate) fn bind_method_code(&self, fn_obj: &mut ObjFn) {
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
                    let value = Value::Class(self.superclass.as_ref().unwrap().clone());
                    // Making this a function call triggers a second mut borrow. :/
                    // fn_obj.set_constant(super_constant, value);
                    fn_obj.constants[super_constant.as_index()] = value;
                }
                Ops::LoadField(field) => *field += field_adjustment(self),
                Ops::StoreField(field) => *field += field_adjustment(self),
                Ops::Closure(constant, _) => {
                    // Bind the nested closure too.
                    let fn_obj = fn_obj.constants[constant.as_index()].try_into_fn().unwrap();
                    self.bind_method_code(&mut fn_obj.borrow_mut());
                }
                _ => {}
            };
        }
    }
}

pub trait Obj: Clear {
    // The object's class.
    fn class_obj(&self) -> Handle<ObjClass>;
}

pub(crate) struct ObjRange {
    pub(crate) class_obj: Handle<ObjClass>,
    // The beginning of the range.
    pub(crate) from: f64,
    // The end of the range. May be greater or less than [from].
    pub(crate) to: f64,
    // True if [to] is included in the range.
    pub(crate) is_inclusive: bool,
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
    pub(crate) class_obj: Handle<ObjClass>,
    pub(crate) data: HashMap<Value, Value>,
}

impl ObjMap {
    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn contains_key(&self, key: &Value) -> bool {
        self.data.contains_key(key)
    }
}

impl Obj for ObjMap {
    fn class_obj(&self) -> Handle<ObjClass> {
        self.class_obj.clone()
    }
}

pub(crate) struct ObjList {
    pub(crate) class_obj: Handle<ObjClass>,
    pub(crate) elements: Vec<Value>,
}

impl ObjList {
    pub fn len(&self) -> usize {
        self.elements.len()
    }
}

impl Obj for ObjList {
    fn class_obj(&self) -> Handle<ObjClass> {
        self.class_obj.clone()
    }
}

pub(crate) struct ObjClosure {
    class_obj: Handle<ObjClass>,
    pub(crate) fn_obj: Handle<ObjFn>,
    upvalues: Vec<Handle<Upvalue>>,
}

impl ObjClosure {
    pub(crate) fn new(vm: &VM, fn_obj: Handle<ObjFn>) -> ObjClosure {
        // FIXME: Is this really supposed to also be class = fn?
        ObjClosure {
            class_obj: vm.fn_class.as_ref().unwrap().clone(),
            fn_obj,
            upvalues: vec![],
        }
    }

    pub fn push_upvalue(&mut self, upvalue: Handle<Upvalue>) {
        self.upvalues.push(upvalue)
    }

    pub fn upvalue(&self, index: usize) -> &Handle<Upvalue> {
        &self.upvalues[index]
    }
}

impl Obj for ObjClosure {
    fn class_obj(&self) -> Handle<ObjClass> {
        self.class_obj.clone()
    }
}

pub(crate) struct ObjFn {
    class_obj: Handle<ObjClass>,
    pub(crate) arity: Arity,
    pub(crate) constants: Vec<Value>,
    pub(crate) code: Vec<Ops>,
    pub(crate) debug: FnDebug,
    // This is needed so that we can find the Module through which to
    // do module-level loads/stores when executing.  This is *definitely*
    // circular reference however.  Maybe there is another way?
    pub(crate) module: Handle<Module>,
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
    pub(crate) fn from_compiler(
        vm: &VM,
        module: Handle<Module>,
        compiler: crate::compiler::Compiler,
        arity: Arity,
    ) -> ObjFn {
        ObjFn {
            class_obj: vm.fn_class.as_ref().unwrap().clone(),
            constants: compiler.constants.list,
            code: compiler.code,
            arity,
            debug: compiler.fn_debug,
            module,
        }
    }

    // pub(crate) fn set_constant(&mut self, constant: &Constant, value: Value) {
    //     self.constants[constant.as_index()] = value;
    // }

    pub(crate) fn lookup_constant(&self, constant: &Constant) -> &Value {
        &self.constants[constant.as_index()]
    }

    pub(crate) fn stub_call(vm: &VM, signature: &str, symbol: Symbol) -> ObjFn {
        let num_params_usize = count_params_in_signature(signature);
        let num_params: u8 = u8::try_from(num_params_usize).unwrap();
        let code = vec![Ops::Call(Arity(num_params), symbol), Ops::Return, Ops::End];
        let code_len = code.len();

        ObjFn {
            class_obj: vm.fn_class.as_ref().unwrap().clone(),
            // FIXME: Why is this Arity + 1 and the above is not?
            arity: Arity(num_params + 1),
            code: code,
            constants: vec![],
            module: vm.last_imported_module.as_ref().unwrap().clone(), // Wrong?
            debug: FnDebug::generated(signature, code_len),
        }
    }
}

impl Obj for ObjFn {
    fn class_obj(&self) -> Handle<ObjClass> {
        self.class_obj.clone()
    }
}

pub(crate) struct ObjInstance {
    class_obj: Handle<ObjClass>,
    pub fields: Vec<Value>,
}

impl ObjInstance {
    pub(crate) fn new(class: Handle<ObjClass>) -> ObjInstance {
        let num_fields = class
            .borrow()
            .num_fields()
            .expect("Compiler emitted Construct for non-source class.");
        let fields = vec![Value::Null; num_fields];
        ObjInstance {
            class_obj: class,
            fields,
        }
    }
}

impl Obj for ObjInstance {
    fn class_obj(&self) -> Handle<ObjClass> {
        self.class_obj.clone()
    }
}

pub(crate) struct ObjForeign {
    class_obj: Handle<ObjClass>,
    pub user_data: Box<dyn UserData>,
}

impl ObjForeign {
    pub(crate) fn new(class: Handle<ObjClass>, user_data: Box<dyn UserData>) -> ObjForeign {
        ObjForeign {
            class_obj: class,
            user_data,
        }
    }
}

impl Obj for ObjForeign {
    fn class_obj(&self) -> Handle<ObjClass> {
        self.class_obj.clone()
    }
}

impl core::fmt::Debug for ObjRange {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let op_string = if self.is_inclusive { ".." } else { "..." };
        write!(f, "{}{}{}", self.from, op_string, self.to)
    }
}

impl Obj for ObjRange {
    fn class_obj(&self) -> Handle<ObjClass> {
        self.class_obj.clone()
    }
}

// impl core::fmt::Debug for dyn Obj {
//     fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
//         write!(f, "Object{:?}", self.obj_type())
//     }
// }

// Derive from Default to allow take() to work during clear().
#[derive(Default)]
pub struct ObjClass {
    // Class is option Option due to Object and Class and Object metaclass
    // initialization starting with class = None and then filling in.
    // The Option is also used during clear() teardown during VM::drop.
    // We could use a dummy class to start intead and make this non-optional.
    pub(crate) class: Option<Handle<ObjClass>>,

    // Class is the only class w/o a superclass, all others this is Some(class).
    pub(crate) superclass: Option<Handle<ObjClass>>,

    // What created this class.  Including the number of fields it has if it
    // was created from source, rather than internal or foreign.
    pub(crate) source: ClassSource,

    // The table of methods that are defined in or inherited by this class.
    // Methods are called by symbol, and the symbol directly maps to an index in
    // this table. This makes method calls fast at the expense of empty cells in
    // the list for methods the class doesn't support.
    //
    // You can think of it as a hash table that never has collisions but has a
    // really low load factor. Since methods are pretty small (just a type and a
    // pointer), this should be a worthwhile trade-off.
    pub(crate) methods: Vec<Option<Method>>,

    // The name of the class.
    pub(crate) name: String, // Should be Rc<ObjString>?

                             // The ClassAttribute for the class, if any
                             //   Value attributes;
}

impl ObjClass {
    fn num_fields(&self) -> Option<usize> {
        match self.source {
            ClassSource::Source(num_fields) => Some(num_fields),
            _ => None, // Not a valid question to ask.
        }
    }

    pub fn is_foreign(&self) -> bool {
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

impl Obj for ObjClass {
    fn class_obj(&self) -> Handle<ObjClass> {
        // FIXME: Fix to be non-Option and remove unwrap.
        self.class.as_ref().unwrap().clone()
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn value_num_hash_unique() {
        fn hash_as_value(num: f64) -> u64 {
            use std::hash::{Hash, Hasher};
            let value = super::Value::Num(num);
            let mut s = std::collections::hash_map::DefaultHasher::new();
            value.hash(&mut s);
            s.finish()
        }
        assert_ne!(hash_as_value(1.0), hash_as_value(2.0));
        assert_eq!(hash_as_value(1.0), hash_as_value(1.0));
        assert_eq!(hash_as_value(3.0), hash_as_value(1.0 + 2.0));
        // Floats equality is hard.  We don't try to fix that:
        assert_ne!(hash_as_value(0.3), hash_as_value(0.1 + 0.2));
    }
}
