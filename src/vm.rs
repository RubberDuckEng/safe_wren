use std::cell::RefCell;
use std::rc::Rc;
use std::str;

use crate::compiler::{Ops, Scope};
use crate::core::{init_core_classes, prim_system_print, register_core_primitives};

#[derive(Clone)]
pub(crate) enum Value {
    Null,
    Num(f64),
    Boolean(bool),
    String(Rc<String>),
    Class(Handle<ObjClass>),
    Range(Handle<ObjRange>),
    // Object(Handle<dyn Obj>),
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

#[derive(Debug)]
pub(crate) struct Function {
    pub constants: Vec<Value>,
    pub code: Vec<Ops>,
}

#[derive(Debug)]
pub(crate) struct Closure {
    pub function: Function,
}

#[derive(Debug)]
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
    MethodNotFound(String),
    ThisObjectHasNoClass,
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

pub struct WrenVM {
    // Current executing module (only module currently)
    pub(crate) module: Module, // No support for multiple modules yet.
    // Stack for this executing VM
    pub(crate) stack: Vec<Value>,
    // Program counter (offset into current code block)
    pc: usize,
    // Print debug information when running
    debug: bool,
    // Single global symbol table for all method names (matches wren_c)
    // Separate Struct to allow easier passing to register_primitive
    pub methods: SymbolTable,
    pub(crate) core: Option<CoreClasses>,
}

impl core::fmt::Debug for WrenVM {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "WrenVM {{ ")?;
        write!(
            f,
            "stack: (len {}, top {:?}), ",
            self.stack.len(),
            self.stack.last()
        )?;
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
fn wren_bind_superclass(subclass: &mut ObjClass, superclass: &Handle<ObjClass>) {
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

fn wren_new_class(
    vm: &mut WrenVM,
    superclass: &Handle<ObjClass>,
    num_fields: usize,
    name: Value,
) -> Result<Handle<ObjClass>, RuntimeError> {
    wren_new_class_with_class_class(
        superclass,
        num_fields,
        name,
        &vm.core.as_ref().unwrap().class,
    )
}

fn as_class(value: Value) -> Handle<ObjClass> {
    match value {
        Value::Class(o) => o,
        _ => panic!(),
    }
}

fn create_class(vm: &mut WrenVM, num_fields: usize) -> Result<(), RuntimeError> {
    // Pull the name and superclass off the stack.
    let superclass = as_class(vm.pop()?);
    let name = vm.pop()?;

    //   vm->fiber->error = validateSuperclass(vm, name, superclass, numFields);

    let class = wren_new_class(vm, &superclass, num_fields, name)?;
    vm.stack.push(Value::Class(class));
    Ok(())
}

type Handle<T> = Rc<RefCell<T>>;

fn new_handle<T>(t: T) -> Handle<T> {
    Rc::new(RefCell::new(t))
}

#[derive(Debug)]
pub(crate) struct CoreClasses {
    pub(crate) num: Handle<ObjClass>,
    pub(crate) bool_class: Handle<ObjClass>,
    pub(crate) null: Handle<ObjClass>,
    pub(crate) string: Handle<ObjClass>,
    pub(crate) class: Handle<ObjClass>,
    pub(crate) range: Handle<ObjClass>,
    // Probably not needed (not in wren_c):
    pub(crate) system: Handle<ObjClass>,
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

// FIXME: This is a hack until we start loading a wren_core.wren
// to define all the core classes for us.
// NOTE: This isn't quite "wren_new_class_with_class_class"
// since that's a interpret-time function and this function does
// both compile time work (declare variables) as well as interpret
// time work (create class objects).
pub(crate) fn base_class(
    module: &mut Module,
    name: &str,
    object_class: &Handle<ObjClass>,
    class_class: &Handle<ObjClass>,
) -> Handle<ObjClass> {
    let metaclass = define_class(module, &format!("{} metaclass", name)); // FIXME
    metaclass.borrow_mut().class = Some(class_class.clone());
    metaclass.borrow_mut().superclass = Some(class_class.clone());

    let class = define_class(module, name);
    class.borrow_mut().class = Some(metaclass.clone());
    class.borrow_mut().superclass = Some(object_class.clone());

    wren_bind_superclass(&mut class.borrow_mut(), object_class);

    class
}

impl WrenVM {
    pub fn new(debug: bool) -> Self {
        let mut vm = Self {
            // dummy_module to avoid changing test results (for now)
            module: Module::with_name("dummy_module"),
            stack: Vec::new(),
            methods: SymbolTable::default(),
            pc: 0,
            debug: debug,
            core: None,
        };

        init_core_classes(&mut vm);
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
        }
    }

    pub(crate) fn run(&mut self, closure: Closure) -> Result<(), RuntimeError> {
        loop {
            let op = &closure.function.code[self.pc];
            self.pc += 1;
            if self.debug {
                self.dump_stack();
                self.dump_instruction(&closure, op);
            }
            match op {
                Ops::Constant(index) => {
                    self.push(closure.function.constants[*index].clone());
                }
                Ops::Boolean(value) => {
                    self.push(Value::Boolean(*value));
                }
                Ops::Null => {
                    self.push(Value::Null);
                }
                Ops::Call(signature) => {
                    // Implicit arg for this.
                    let num_args = signature.arity as usize + 1;
                    let this_offset = self.stack.len() - num_args;
                    let args = self.stack.split_off(this_offset);

                    if signature.full_name.eq("print(_)") {
                        self.push(prim_system_print(self, args)?);
                    } else {
                        let this_class = self
                            .class_for_value(&args[0])
                            .ok_or(RuntimeError::ThisObjectHasNoClass)?;
                        let symbol = self
                            .methods
                            .lookup(&signature.full_name)
                            .ok_or(RuntimeError::MethodNotFound(signature.full_name.clone()))?;

                        let class_obj = this_class.borrow();
                        let method = class_obj
                            .methods
                            .get(symbol)
                            .ok_or(RuntimeError::MethodNotFound(signature.full_name.clone()))?;

                        match method {
                            Method::Primitive(f) => {
                                let result = f(self, args)?;
                                // When do we remove args from stack?
                                self.stack.push(result);
                            }
                            Method::None => unimplemented!(),
                        }
                    }
                }
                Ops::Class(num_fields) => {
                    // FIXME: Pass module?
                    create_class(self, *num_fields)?;
                }
                Ops::Load(variable) => {
                    let value = match variable.scope {
                        Scope::Module => self.module.variables[variable.index].clone(),
                        Scope::Upvalue => unimplemented!("load upvalue"),
                        Scope::Local => self.stack[variable.index].clone(),
                    };
                    self.push(value);
                }
                Ops::Store(variable) => {
                    let value = self.peek()?;
                    match variable.scope {
                        Scope::Module => self.module.variables[variable.index] = value.clone(),
                        Scope::Upvalue => unimplemented!("store upvalue"),
                        Scope::Local => self.stack[variable.index] = value.clone(),
                    };
                }
                Ops::Pop => {
                    self.pop()?;
                }
                Ops::End => {
                    return Ok(());
                }
                Ops::Loop(offset_backwards) => {
                    self.pc -= *offset_backwards as usize;
                }
                Ops::Jump(offset_forward) => {
                    self.pc += *offset_forward as usize;
                }
                Ops::JumpIfFalse(offset_forward) => {
                    let value = self.pop()?;
                    if value.is_falsey() {
                        self.pc += *offset_forward as usize;
                    }
                }
                Ops::And(offset_forward) => {
                    // This differs from JumpIfFalse in whether it pops
                    let value = self.peek()?;
                    if value.is_falsey() {
                        self.pc += *offset_forward as usize;
                    } else {
                        self.pop()?;
                    }
                }
                Ops::Or(offset_forward) => {
                    let value = self.peek()?;
                    if !value.is_falsey() {
                        self.pc += *offset_forward as usize;
                    } else {
                        self.pop()?;
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

impl WrenVM {
    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Result<Value, RuntimeError> {
        self.stack.pop().ok_or(RuntimeError::StackUnderflow)
    }

    fn peek(&mut self) -> Result<&Value, RuntimeError> {
        self.stack.last().ok_or(RuntimeError::StackUnderflow)
    }

    fn dump_instruction(&self, closure: &Closure, op: &Ops) {
        let string = match op {
            Ops::Constant(i) => format!("Constant ({}: {:?})", i, closure.function.constants[*i]),
            Ops::Boolean(b) => format!("Boolean {}", b),
            Ops::Null => format!("{:?}", op),
            Ops::Call(_) => format!("{:?}", op),
            Ops::Load(v) => match v.scope {
                Scope::Local => {
                    format!("Load Local {}: {:?}", v.index, self.stack[v.index])
                }
                Scope::Upvalue => unimplemented!("dump load upvalue"),
                Scope::Module => {
                    format!(
                        "Load Module {}: {:?}",
                        v.index, self.module.variables[v.index]
                    )
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
            Ops::Class(_) => format!("{:?}", op),
            Ops::Jump(_) => format!("{:?}", op),
            Ops::Loop(_) => format!("{:?}", op),
            Ops::Pop => format!("{:?}", op),
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
            Ops::Jump(_) => format!("{:?}", self),
            Ops::Loop(_) => format!("{:?}", self),
            Ops::Pop => format!("{:?}", self),
            Ops::End => format!("{:?}", self),
            Ops::And(_) => format!("{:?}", self),
            Ops::AndPlaceholder => format!("{:?}", self),
            Ops::Or(_) => format!("{:?}", self),
            Ops::OrPlaceholder => format!("{:?}", self),
        }
    }
}

pub(crate) fn debug_bytecode(_vm: &WrenVM, closure: &Closure) {
    println!("{:?}", closure);
    let ops = closure.function.code.iter();
    ops.enumerate().for_each(|(i, op)| {
        println!("{:02}: {}", i, op.debug_string());
    })
}

// Identifies which specific type a heap-allocated object is.
#[derive(Debug)]
pub enum ObjType {
    Class,
    // Closure,
    // Fiber,
    // Function,
    // Foreign,
    // Instance,
    // List,
    // Map,
    // Module,
    Range,
    // String,
    // Upvalue,
}

// Base struct for all heap-allocated objects.
pub trait Obj {
    fn obj_type(&self) -> ObjType;
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

impl core::fmt::Debug for ObjRange {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let op_string = if self.is_inclusive { ".." } else { "..." };
        write!(f, "{}{}{}", self.from, op_string, self.to)
    }
}

impl Obj for ObjRange {
    fn obj_type(&self) -> ObjType {
        ObjType::Range
    }
    fn class_obj(&self) -> Option<Handle<ObjClass>> {
        Some(self.class_obj.clone())
    }
}

impl core::fmt::Debug for dyn Obj {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "Object{:?}", self.obj_type())
    }
}

// Unclear if this should take Vec or a slice?
type Primitive = fn(vm: &WrenVM, args: Vec<Value>) -> Result<Value, RuntimeError>;

#[derive(Clone)]
pub(crate) enum Method {
    // A primitive method implemented in C in the VM. Unlike foreign methods,
    // this can directly manipulate the fiber's stack.
    Primitive(Primitive),

    // A primitive that handles .call on Fn.
    //   FunctionCall,

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
    fn obj_type(&self) -> ObjType {
        ObjType::Class
    }

    fn class_obj(&self) -> Option<Handle<ObjClass>> {
        self.class.clone()
    }
}
