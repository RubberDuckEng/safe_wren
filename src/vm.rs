use std::cell::RefCell;
use std::rc::Rc;
use std::str;

use crate::compiler::{compile, InputManager, Ops, Scope, Signature};
use crate::core::{init_base_classes, register_core_primitives};

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
    // Object(Handle<dyn Obj>),
}

impl PartialEq for Value {
    fn eq(&self, rhs: &Value) -> bool {
        match (self, rhs) {
            (Value::Null, Value::Null) => return true,
            (Value::Num(a), Value::Num(b)) => return a == b,
            (Value::Boolean(a), Value::Boolean(b)) => return a == b,
            (Value::Class(a), Value::Class(b)) => return a == b,
            (Value::Range(a_range), Value::Range(b_range)) => {
                let a = a_range.borrow();
                let b = b_range.borrow();
                return a.from == b.from && a.to == b.to && a.is_inclusive == b.is_inclusive;
            }
            (Value::String(a_string), Value::String(b_string)) => return a_string.eq(&b_string),
            _ => return false,
        }
    }
}

impl core::fmt::Debug for Value {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Value::Null => write!(f, "Null"),
            Value::Num(n) => write!(f, "Num({})", n),
            Value::Boolean(b) => write!(f, "Boolean({})", b),
            Value::String(s) => write!(f, "String(\"{}\")", s),
            Value::Class(c) => write!(f, "Class(\"{}\")", c.borrow().name),
            Value::Range(r) => write!(f, "Range({:?})", r),
            Value::Fn(_) => write!(f, "Fn()"),
            Value::Closure(_) => write!(f, "Closure()"),
        }
    }
}

impl Value {
    pub(crate) fn from_string(string: String) -> Value {
        Value::String(Rc::new(string))
    }

    // Different from "is truthy" used by "as_bool" in wren_c
    // Unclear if this is only ever called on a known-bool?
    pub(crate) fn equals_true(&self) -> bool {
        match self {
            Value::Boolean(b) => *b == true,
            _ => false,
        }
    }

    // In Wren false and null are false, everything else is true.
    fn is_truthy(&self) -> bool {
        match self {
            Value::Null => false,
            Value::Boolean(b) => *b,
            _ => true,
        }
    }

    fn is_falsey(&self) -> bool {
        !self.is_truthy()
    }

    pub(crate) fn is_null(&self) -> bool {
        match self {
            Value::Null => true,
            _ => false,
        }
    }
}

#[derive(Default, Debug)]
pub(crate) struct Module {
    // Missing some pointer to the actual code?

    // Should this just be a map?  wren_utils.h suggests so?
    variables: Vec<Value>,
    variable_names: Vec<String>,
    pub name: String, // Should be a GC'd object?
}

impl Module {
    pub fn with_name(name: &str) -> Module {
        Module {
            name: name.into(),
            ..Module::default()
        }
    }

    pub fn lookup_symbol(&self, name: &str) -> Option<u16> {
        self.variable_names
            .iter()
            .position(|e| e.eq(name))
            .map(|s| s as u16)
    }

    pub fn define_variable(&mut self, name: &str, value: Value) -> usize {
        self.variable_names.push(name.into());
        self.variables.push(value);
        self.variable_names.len() - 1
    }
}

pub(crate) fn find_core_class(vm: &WrenVM, name: &str) -> Handle<ObjClass> {
    let symbol = vm
        .module
        .lookup_symbol(name)
        .expect(&format!("find_core_class failed to load {}", name));
    vm.module.variables[symbol as usize]
        .try_into_class()
        .expect(&format!("find_core_class failed to load {}", name))
}

#[derive(Debug)]
pub(crate) struct Function {
    pub constants: Vec<Value>,
    pub code: Vec<Ops>,
}

#[derive(Debug)]
pub(crate) struct MissingMethod {
    this_class: String,
    name: String,
}

pub(crate) enum RuntimeError {
    StackUnderflow,
    // VariableAlreadyDefined,
    // TooManyVariablesDefined,
    // VariableUsedBeforeDefinition,
    NumberRequired(Value),
    StringRequired(Value),
    ObjectRequired(Value),
    ClassRequired(Value),
    RangeRequired(Value),
    ClosureRequired(Value),
    MethodNotFound(MissingMethod),
    ThisObjectHasNoClass,
}

impl core::fmt::Debug for RuntimeError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            RuntimeError::StackUnderflow => write!(f, "StackUnderflow"),
            RuntimeError::NumberRequired(v) => write!(f, "NumberRequired({:?})", v),
            RuntimeError::StringRequired(v) => write!(f, "StringRequired({:?})", v),
            RuntimeError::ObjectRequired(v) => write!(f, "ObjectRequired({:?})", v),
            RuntimeError::ClassRequired(v) => write!(f, "ClassRequired({:?})", v),
            RuntimeError::RangeRequired(v) => write!(f, "RangeRequired({:?})", v),
            RuntimeError::ClosureRequired(v) => write!(f, "ClosureRequired({:?})", v),
            RuntimeError::MethodNotFound(m) => {
                write!(f, "MethodNotFound(\"{}\" on \"{}\")", m.name, m.this_class)
            }
            RuntimeError::ThisObjectHasNoClass => write!(f, "ThisObjectHasNoClass"),
        }
    }
}

impl Value {
    pub fn try_into_num(&self) -> Result<f64, RuntimeError> {
        match self {
            Value::Num(value) => Ok(*value),
            _ => Err(RuntimeError::NumberRequired(self.clone())),
        }
    }

    pub fn try_into_string(&self) -> Result<String, RuntimeError> {
        match self {
            Value::String(string) => Ok(string.as_ref().clone()),
            _ => Err(RuntimeError::StringRequired(self.clone())),
        }
    }

    pub fn try_into_class(&self) -> Result<Handle<ObjClass>, RuntimeError> {
        match self {
            Value::Class(c) => Ok(c.clone()),
            _ => Err(RuntimeError::ClassRequired(self.clone())),
        }
    }

    pub fn try_into_range(&self) -> Result<Handle<ObjRange>, RuntimeError> {
        match self {
            Value::Range(r) => Ok(r.clone()),
            _ => Err(RuntimeError::RangeRequired(self.clone())),
        }
    }

    pub fn try_into_fn(&self) -> Result<Handle<ObjFn>, RuntimeError> {
        match self {
            Value::Fn(c) => Ok(c.clone()),
            _ => Err(RuntimeError::ClosureRequired(self.clone())),
        }
    }

    pub fn try_into_closure(&self) -> Result<Handle<ObjClosure>, RuntimeError> {
        match self {
            Value::Closure(c) => Ok(c.clone()),
            _ => Err(RuntimeError::ClosureRequired(self.clone())),
        }
    }
}

#[derive(Debug, Default)]
pub struct SymbolTable {
    method_names: Vec<String>,
}

impl SymbolTable {
    pub fn ensure_method(&mut self, name: &str) -> usize {
        if let Some(index) = self.method_names.iter().position(|n| n.eq(name)) {
            return index;
        }

        // New symbol, so add it.
        self.method_names.push(name.into());
        self.method_names.len() - 1
    }

    pub fn lookup(&self, name: &str) -> Option<usize> {
        self.method_names.iter().position(|n| n.eq(name))
    }
}

pub struct CallFrame {
    // Program counter (offset into current code block)
    pc: usize,
    // The closure being executed.
    closure: Handle<ObjClosure>,
    stack: Vec<Value>,
}

impl CallFrame {
    fn new(closure: Handle<ObjClosure>) -> CallFrame {
        CallFrame {
            pc: 0,
            closure: closure,
            stack: Vec::new(),
        }
    }
}

impl core::fmt::Debug for CallFrame {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(
            f,
            "stack: (len {}, top {:?}), ",
            self.stack.len(),
            self.stack.last()
        )
    }
}

pub struct Fiber {
    call_stack: Vec<CallFrame>,
}

impl Fiber {
    fn new(closure: Handle<ObjClosure>) -> Fiber {
        Fiber {
            call_stack: vec![CallFrame::new(closure)],
        }
    }
}
impl core::fmt::Debug for Fiber {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(
            f,
            "call_stack: (len {}, top {:?}), ",
            self.call_stack.len(),
            self.call_stack.last()
        )
    }
}

pub struct WrenVM {
    // Current executing module (only module currently)
    pub(crate) module: Module, // No support for multiple modules yet.
    // Current executing Fiber (should eventually be a list?)
    last_fiber: Option<Fiber>,
    // Print debug information when running
    debug: bool,
    // Single global symbol table for all method names (matches wren_c)
    // Separate Struct to allow easier passing to register_primitive
    pub methods: SymbolTable,
    pub(crate) class_class: Option<Handle<ObjClass>>,
    pub(crate) fn_class: Option<Handle<ObjClass>>,
    pub(crate) core: Option<CoreClasses>,
}

impl core::fmt::Debug for WrenVM {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "WrenVM {{ ")?;
        if let Some(fiber) = &self.last_fiber {
            write!(f, "stack: {:?}, ", fiber)?;
        }
        write!(f, "methods: (len {}) ", self.methods.method_names.len())?;
        write!(f, "}}")
    }
}

pub(crate) fn wren_new_range(
    vm: &WrenVM,
    from: f64,
    to: f64,
    is_inclusive: bool,
) -> Handle<ObjRange> {
    new_handle(ObjRange {
        class_obj: vm.core.as_ref().unwrap().range.clone(),
        from: from,
        to: to,
        is_inclusive: is_inclusive,
    })
}
pub(crate) fn wren_bind_superclass(subclass: &mut ObjClass, superclass: &Handle<ObjClass>) {
    subclass.superclass = Some(superclass.clone());
    // Setup fields
    // Inherit methods

    // Inherit methods from its superclass.
    // FIXME: Should this be in reverse order (to minimize # of resizes?)
    for (symbol, method) in superclass.borrow().methods.iter().enumerate() {
        subclass.set_method(symbol, method.clone());
    }
}

fn wren_new_single_class(_num_fields: usize, name: String) -> Handle<ObjClass> {
    // the wren_c version does a lot more?  Unclear if this should.
    new_handle(ObjClass {
        name: name,
        methods: Vec::new(),
        class: None,
        superclass: None,
    })
}

// This was made in hopes of sharing code with base_class but it turns
// out this is an interpret time function (only creates classes)
// and does not do any of the declaration work base_class does.
// Keeping it for now in case it's useful later.
fn wren_new_class_with_class_class(
    superclass: &Handle<ObjClass>,
    num_fields: usize,
    name: Value,
    class_class: &Handle<ObjClass>,
) -> Result<Handle<ObjClass>, RuntimeError> {
    // Create the metaclass.

    let name_string = name.try_into_string()?;
    let metaclass_name_string = format!("{} metaclass", name_string);
    // let metaclass_name = Value::String(Rc::new(metaclass_name_string));

    let metaclass = wren_new_single_class(0, metaclass_name_string);
    metaclass.borrow_mut().class = Some(class_class.clone());

    // Metaclasses always inherit Class and do not parallel the non-metaclass
    // hierarchy.
    wren_bind_superclass(&mut metaclass.borrow_mut(), class_class);

    let class = wren_new_single_class(num_fields, name_string);
    class.borrow_mut().class = Some(metaclass);
    wren_bind_superclass(&mut class.borrow_mut(), superclass);

    Ok(class)
}

pub(crate) fn wren_new_class(
    vm: &WrenVM,
    superclass: &Handle<ObjClass>,
    num_fields: usize,
    name: Value,
) -> Result<Handle<ObjClass>, RuntimeError> {
    wren_new_class_with_class_class(
        superclass,
        num_fields,
        name,
        &vm.class_class.as_ref().unwrap(),
    )
}

fn as_class(value: Value) -> Handle<ObjClass> {
    match value {
        Value::Class(o) => o,
        _ => panic!(),
    }
}

fn create_class(vm: &WrenVM, frame: &mut CallFrame, num_fields: usize) -> Result<(), RuntimeError> {
    // Pull the name and superclass off the stack.
    let superclass = as_class(frame.pop()?);
    let name = frame.pop()?;

    //   vm->fiber->error = validateSuperclass(vm, name, superclass, numFields);

    let class = wren_new_class(vm, &superclass, num_fields, name)?;
    frame.push(Value::Class(class));
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
}

#[derive(Debug)]
pub enum ModuleError {
    VariableAlreadyDefined,
}

pub(crate) fn wren_define_variable(
    module: &mut Module,
    name: &str,
    value: Value,
) -> Result<usize, ModuleError> {
    // See if the variable is already explicitly or implicitly declared.
    match module.lookup_symbol(name) {
        None => {
            // New variable!
            Ok(module.define_variable(name, value))
        }
        Some(_) => {
            Err(ModuleError::VariableAlreadyDefined)
            // FIXME: Handle fixing implicit variables.
            // Which are currently their line numer?
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
    });

    wren_define_variable(module, name, Value::Class(class.clone())).expect("defined");
    class
}

fn method_not_found(class: &ObjClass, signature: &Signature) -> RuntimeError {
    RuntimeError::MethodNotFound(MissingMethod {
        this_class: class.name.clone(),
        name: signature.full_name.clone(),
    })
}

pub(crate) fn load_wren_core(vm: &mut WrenVM) {
    let had_debug = vm.debug;
    vm.debug = false;
    use std::fs;
    // hacks upon hacks.
    let path = "stub_wren_core.wren";
    let source = fs::read_to_string(path).unwrap_or_else(|e| {
        panic!("Failed to open file \"{}\": {}", path, e);
    });

    let input = InputManager::from_string(source);
    let closure = compile(vm, input, "core".into()).expect("compile wren_core");
    vm.run(closure).expect("run wren_core");
    vm.debug = had_debug;
}

enum RunNext {
    FunctionCall(CallFrame),
    FunctionReturn(Value),
    Done,
}

impl WrenVM {
    pub fn new(debug: bool) -> Self {
        let mut vm = Self {
            // dummy_module to avoid changing test results (for now)
            module: Module::with_name("dummy_module"),
            methods: SymbolTable::default(),
            last_fiber: None,
            debug: debug,
            core: None,
            class_class: None,
            fn_class: None,
        };

        init_base_classes(&mut vm);
        load_wren_core(&mut vm);
        register_core_primitives(&mut vm);
        vm
    }

    // TODO: This needs to be Option?  Root classes dont have classes?
    pub(crate) fn class_for_value(&self, value: &Value) -> Option<Handle<ObjClass>> {
        let core = self.core.as_ref().unwrap();
        match value {
            Value::Null => Some(core.null.clone()),
            Value::Num(_) => Some(core.num.clone()),
            Value::Boolean(_) => Some(core.bool_class.clone()),
            Value::String(_) => Some(core.string.clone()),
            Value::Class(o) => o.borrow().class_obj(),
            Value::Range(o) => o.borrow().class_obj(),
            Value::Fn(o) => o.borrow().class_obj(),
            Value::Closure(o) => o.borrow().class_obj(),
        }
    }

    #[allow(dead_code)]
    fn print_methods(&self, class: &ObjClass) {
        println!("{:?} has:", class);
        for (symbol, method) in class.methods.iter().enumerate() {
            match method {
                Method::None => (),
                _ => {
                    let name = &self.methods.method_names[symbol];
                    println!("{} ({}) => {:?}", name, symbol, method);
                }
            }
        }
    }

    pub(crate) fn run(&mut self, closure: Handle<ObjClosure>) -> Result<Value, RuntimeError> {
        let mut fiber = Fiber::new(closure);
        loop {
            let mut frame = fiber.call_stack.pop().unwrap();
            // This is all to avoid run_fiber needing to call itself
            // recursively, or the run_fiber main loop needing to pull
            // the frame on every iteration.  Maybe not worth it?
            match self.run_frame_in_fiber(&mut frame, &fiber) {
                Ok(RunNext::FunctionCall(new_frame)) => fiber.call_stack.push(new_frame),
                Ok(RunNext::FunctionReturn(value)) => {
                    if fiber.call_stack.is_empty() {
                        return Ok(value);
                    } else {
                        // Take the return value and push it onto the calling stack.
                        fiber.call_stack.last_mut().unwrap().push(value);
                    }
                }
                Ok(RunNext::Done) => {
                    // FIXME: Is this an error now that FunctionReturn is implemented?
                    self.last_fiber = Some(fiber);
                    return Ok(Value::Null);
                }
                Err(e) => {
                    self.last_fiber = Some(fiber);
                    return Err(e);
                }
            }
        }
    }

    fn run_frame_in_fiber(
        &mut self,
        frame: &mut CallFrame,
        _fiber: &Fiber,
    ) -> Result<RunNext, RuntimeError> {
        let fn_obj = frame.closure.borrow().fn_obj.clone();
        let mut pc = frame.pc;
        loop {
            let op = &fn_obj.borrow().function.code[pc];
            pc += 1;
            if self.debug {
                frame.dump_stack();
                frame.dump_instruction(&self.module, &frame.closure.borrow(), op);
            }
            match op {
                Ops::Constant(index) => {
                    frame.push(fn_obj.borrow().function.constants[*index].clone());
                }
                Ops::Boolean(value) => {
                    frame.push(Value::Boolean(*value));
                }
                Ops::Null => {
                    frame.push(Value::Null);
                }
                Ops::Call(signature) => {
                    // Implicit arg for this.
                    let num_args = signature.arity as usize + 1;
                    let this_offset = frame.stack.len() - num_args;
                    // Unlike wren_c, we remove the args from the stack
                    // and pass them to the function.
                    let args = frame.stack.split_off(this_offset);

                    let this_class = self
                        .class_for_value(&args[0])
                        .ok_or(RuntimeError::ThisObjectHasNoClass)?;
                    let class_obj = this_class.borrow();

                    // Get the global usize for this function name
                    let symbol = self
                        .methods
                        .lookup(&signature.full_name)
                        .ok_or(method_not_found(&class_obj, &signature))?;

                    // Get the Method record for this class for this symbol.
                    // This could return None instead of MethodNone?
                    let method = class_obj
                        .methods
                        .get(symbol)
                        .ok_or(method_not_found(&class_obj, &signature))?;

                    // Even if we got a Method doesn't mean *this* class
                    // implements it.
                    match method {
                        Method::Primitive(f) => {
                            let result = f(self, args)?;
                            frame.push(result);
                        }
                        Method::FunctionCall => {
                            // Pushes a new stack frame.
                            // wren_rust (currently) keeps a separate stack
                            // per CallFrame, so underflows are caught on a
                            // per-function call basis.
                            return Ok(RunNext::FunctionCall(CallFrame {
                                pc: 0,
                                closure: args[0].try_into_closure()?,
                                stack: args,
                            }));
                        }
                        Method::None => {
                            // This is the case where the methods vector was
                            // large enough to have the symbol (because the
                            // class supports a larger symbol), but this symbol
                            // is Method::None (not implemented).
                            return Err(method_not_found(&class_obj, &signature));
                        }
                    }
                }
                Ops::Closure(constant_index, _upvalues) => {
                    let fn_value = fn_obj.borrow().function.constants[*constant_index].clone();
                    let fn_obj = fn_value.try_into_fn()?;
                    let closure = new_handle(ObjClosure::new(self, fn_obj));
                    frame.push(Value::Closure(closure));
                    // FIXME: Handle upvalues.
                }
                Ops::Return => {
                    // The top of our stack is returned and then the caller of
                    // this rust function will push the return onto the stack of
                    // the calling wren function CallFrame.
                    return Ok(RunNext::FunctionReturn(frame.pop()?));
                }
                Ops::Class(num_fields) => {
                    // FIXME: Pass module?
                    create_class(self, frame, *num_fields)?;
                }
                Ops::Load(variable) => {
                    let value = match variable.scope {
                        Scope::Module => self.module.variables[variable.index].clone(),
                        Scope::Upvalue => unimplemented!("load upvalue"),
                        Scope::Local => frame.stack[variable.index].clone(),
                    };
                    frame.push(value);
                }
                Ops::Store(variable) => {
                    let value = frame.peek()?;
                    match variable.scope {
                        Scope::Module => self.module.variables[variable.index] = value.clone(),
                        Scope::Upvalue => unimplemented!("store upvalue"),
                        Scope::Local => frame.stack[variable.index] = value.clone(),
                    };
                }
                Ops::Pop => {
                    frame.pop()?;
                }
                Ops::End => {
                    return Ok(RunNext::Done);
                }
                Ops::Loop(offset_backwards) => {
                    pc -= *offset_backwards as usize;
                }
                Ops::Jump(offset_forward) => {
                    pc += *offset_forward as usize;
                }
                Ops::JumpIfFalse(offset_forward) => {
                    let value = frame.pop()?;
                    if value.is_falsey() {
                        pc += *offset_forward as usize;
                    }
                }
                Ops::And(offset_forward) => {
                    // This differs from JumpIfFalse in whether it pops
                    let value = frame.peek()?;
                    if value.is_falsey() {
                        pc += *offset_forward as usize;
                    } else {
                        frame.pop()?;
                    }
                }
                Ops::Or(offset_forward) => {
                    let value = frame.peek()?;
                    if !value.is_falsey() {
                        pc += *offset_forward as usize;
                    } else {
                        frame.pop()?;
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

impl CallFrame {
    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Result<Value, RuntimeError> {
        self.stack.pop().ok_or(RuntimeError::StackUnderflow)
    }

    fn peek(&mut self) -> Result<&Value, RuntimeError> {
        self.stack.last().ok_or(RuntimeError::StackUnderflow)
    }

    fn dump_instruction(&self, module: &Module, closure: &ObjClosure, op: &Ops) {
        let string = match op {
            Ops::Constant(i) => format!(
                "Constant ({}: {:?})",
                i,
                closure.fn_obj.borrow().function.constants[*i]
            ),
            Ops::Boolean(b) => format!("Boolean {}", b),
            Ops::Null => format!("{:?}", op),
            Ops::Call(_) => format!("{:?}", op),
            Ops::Load(v) => match v.scope {
                Scope::Local => {
                    format!("Load Local {}: {:?}", v.index, self.stack[v.index])
                }
                Scope::Upvalue => unimplemented!("dump load upvalue"),
                Scope::Module => {
                    format!("Load Module {}: {:?}", v.index, module.variables[v.index])
                }
            },
            Ops::Store(_) => format!("{:?}", op),
            Ops::JumpIfFalsePlaceholder => format!("{:?}", op),
            Ops::JumpIfFalse(_) => format!("{:?}", op),
            Ops::JumpPlaceholder => format!("{:?}", op),
            Ops::And(_) => format!("{:?}", op),
            Ops::AndPlaceholder => format!("{:?}", op),
            Ops::Or(_) => format!("{:?}", op),
            Ops::OrPlaceholder => format!("{:?}", op),
            Ops::ClassPlaceholder => format!("{:?}", op),
            Ops::Closure(_, _) => format!("{:?}", op),
            Ops::Class(_) => format!("{:?}", op),
            Ops::Jump(_) => format!("{:?}", op),
            Ops::Loop(_) => format!("{:?}", op),
            Ops::Pop => format!("{:?}", op),
            Ops::Return => format!("{:?}", op),
            Ops::End => format!("{:?}", op),
        };
        println!("{}", string);
    }

    fn dump_stack(&self) {
        // Print the stack left (top) to right (bottom)
        let mut as_string = Vec::new();
        for value in &self.stack {
            as_string.push(format!("{:?}", value));
        }
        as_string.reverse();
        println!("  Stack: [{}]", as_string.join(", "));
    }
}

impl Ops {
    fn debug_string(&self) -> String {
        match self {
            Ops::Constant(_) => format!("{:?}", self),
            Ops::Boolean(_) => format!("{:?}", self),
            Ops::Null => format!("{:?}", self),
            Ops::Call(_) => format!("{:?}", self),
            Ops::Load(_) => format!("{:?}", self),
            Ops::Store(_) => format!("{:?}", self),
            Ops::JumpIfFalsePlaceholder => format!("{:?}", self),
            Ops::JumpIfFalse(_) => format!("{:?}", self),
            Ops::JumpPlaceholder => format!("{:?}", self),
            Ops::ClassPlaceholder => format!("{:?}", self),
            Ops::Class(_) => format!("{:?}", self),
            Ops::Closure(_, _) => format!("{:?}", self),
            Ops::Jump(_) => format!("{:?}", self),
            Ops::Loop(_) => format!("{:?}", self),
            Ops::Pop => format!("{:?}", self),
            Ops::Return => format!("{:?}", self),
            Ops::End => format!("{:?}", self),
            Ops::And(_) => format!("{:?}", self),
            Ops::AndPlaceholder => format!("{:?}", self),
            Ops::Or(_) => format!("{:?}", self),
            Ops::OrPlaceholder => format!("{:?}", self),
        }
    }
}

pub(crate) fn debug_bytecode(_vm: &WrenVM, closure: &ObjClosure) {
    let func = &closure.fn_obj.borrow().function;
    println!("{:?}", func);
    let ops = func.code.iter();
    ops.enumerate().for_each(|(i, op)| {
        println!("{:02}: {}", i, op.debug_string());
    })
}

// // Identifies which specific type a heap-allocated object is.
// #[derive(Debug)]
// pub enum ObjType {
//     Class,
//     // Closure,
//     // Fiber,
//     // Function,
//     // Foreign,
//     // Instance,
//     // List,
//     // Map,
//     // Module,
//     Range,
//     // String,
//     // Upvalue,
// }

// Base struct for all heap-allocated objects.
pub trait Obj {
    // fn obj_type(&self) -> ObjType;
    //   // The object's class.
    fn class_obj(&self) -> Option<Handle<ObjClass>>;
}

pub(crate) struct ObjRange {
    class_obj: Handle<ObjClass>,
    // The beginning of the range.
    pub(crate) from: f64,
    // The end of the range. May be greater or less than [from].
    pub(crate) to: f64,
    // True if [to] is included in the range.
    pub(crate) is_inclusive: bool,
}

pub(crate) struct ObjClosure {
    class_obj: Handle<ObjClass>,
    pub(crate) fn_obj: Handle<ObjFn>,
}

impl ObjClosure {
    pub(crate) fn new(vm: &WrenVM, fn_obj: Handle<ObjFn>) -> ObjClosure {
        // FIXME: Is this really supposed to also be class = fn?
        ObjClosure {
            class_obj: vm.fn_class.as_ref().unwrap().clone(),
            fn_obj: fn_obj,
        }
    }
}

impl Obj for ObjClosure {
    // fn obj_type(&self) -> ObjType {
    //     ObjType::Fn
    // }
    fn class_obj(&self) -> Option<Handle<ObjClass>> {
        Some(self.class_obj.clone())
    }
}

pub(crate) struct ObjFn {
    class_obj: Handle<ObjClass>,
    pub(crate) arity: u8,
    // Flatten this into ObjFn later?
    pub(crate) function: Function,
}

impl ObjFn {
    pub(crate) fn new(vm: &WrenVM, function: Function, arity: u8) -> ObjFn {
        ObjFn {
            class_obj: vm.fn_class.as_ref().unwrap().clone(),
            function: function,
            arity: arity,
        }
    }
}

impl Obj for ObjFn {
    // fn obj_type(&self) -> ObjType {
    //     ObjType::Fn
    // }
    fn class_obj(&self) -> Option<Handle<ObjClass>> {
        Some(self.class_obj.clone())
    }
}

impl core::fmt::Debug for ObjRange {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let op_string = if self.is_inclusive { ".." } else { "..." };
        write!(f, "{}{}{}", self.from, op_string, self.to)
    }
}

impl Obj for ObjRange {
    // fn obj_type(&self) -> ObjType {
    //     ObjType::Range
    // }
    fn class_obj(&self) -> Option<Handle<ObjClass>> {
        Some(self.class_obj.clone())
    }
}

// impl core::fmt::Debug for dyn Obj {
//     fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
//         write!(f, "Object{:?}", self.obj_type())
//     }
// }

// Unclear if this should take Vec or a slice?
type Primitive = fn(vm: &WrenVM, args: Vec<Value>) -> Result<Value, RuntimeError>;

#[derive(Clone)]
pub(crate) enum Method {
    // A primitive method implemented in C in the VM. Unlike foreign methods,
    // this can directly manipulate the fiber's stack.
    Primitive(Primitive),

    // A primitive that handles .call on Fn.
    FunctionCall,

    // A externally-defined C method.
    //   ForeignFunction,

    // A normal user-defined method.
    // Block

    // No method for the given symbol.
    None,
}

impl core::fmt::Debug for Method {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Method::Primitive(_) => write!(f, "Primitive"),
            Method::FunctionCall => write!(f, "FunctionCall"),
            Method::None => write!(f, "None"),
        }
    }
}

pub struct ObjClass {
    pub(crate) class: Option<Handle<ObjClass>>,
    pub(crate) superclass: Option<Handle<ObjClass>>,

    // The number of fields needed for an instance of this class, including all
    // of its superclass fields.
    //   int numFields;

    // The table of methods that are defined in or inherited by this class.
    // Methods are called by symbol, and the symbol directly maps to an index in
    // this table. This makes method calls fast at the expense of empty cells in
    // the list for methods the class doesn't support.
    //
    // You can think of it as a hash table that never has collisions but has a
    // really low load factor. Since methods are pretty small (just a type and a
    // pointer), this should be a worthwhile trade-off.
    methods: Vec<Method>,

    // The name of the class.
    pub(crate) name: String, // Should be Rc<ObjString>?

                             // The ClassAttribute for the class, if any
                             //   Value attributes;
}

// FIXME: This is a hack?
impl PartialEq for ObjClass {
    fn eq(&self, other: &ObjClass) -> bool {
        self.name.eq(&other.name)
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
    pub(crate) fn set_method(&mut self, symbol: usize, method: Method) {
        if symbol >= self.methods.len() {
            self.methods.resize(symbol + 1, Method::None);
        }
        self.methods[symbol] = method;
    }
}

impl Obj for ObjClass {
    // fn obj_type(&self) -> ObjType {
    //     ObjType::Class
    // }

    fn class_obj(&self) -> Option<Handle<ObjClass>> {
        self.class.clone()
    }
}
