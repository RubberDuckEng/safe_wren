use std::cell::RefCell;
use std::rc::Rc;
use std::str;

use crate::compiler::{Ops, Scope};
use crate::core::{prim_system_print, register_core_primitives};

#[derive(Debug, Clone)]
pub enum Value {
    Null,
    Num(f64),
    Boolean(bool),
    String(Rc<String>),
    Object(Rc<dyn Obj>),
}

impl Value {
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
}

#[derive(Default, Debug)]
pub struct Module {
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

    pub fn lookup_symbol(&self, name: &str) -> Option<usize> {
        self.variable_names.iter().position(|e| e.eq(name))
    }

    pub fn define_variable(&mut self, name: &str, value: Value) {
        self.variable_names.push(name.into());
        self.variables.push(value);
    }
}

#[derive(Debug)]
pub struct Function {
    pub constants: Vec<Value>,
    pub code: Vec<Ops>,
}

#[derive(Debug)]
pub struct Closure {
    pub function: Function,
}

#[derive(Debug)]
pub enum RuntimeError {
    StackUnderflow,
    // VariableAlreadyDefined,
    // TooManyVariablesDefined,
    // VariableUsedBeforeDefinition,
    NumberRequired(Value),
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

#[derive(Debug)]
pub struct WrenVM {
    // Current executing module (only module currently)
    pub module: Module, // No support for multiple modules yet.
    // Stack for this executing VM
    pub stack: Vec<Value>,
    // Program counter (offset into current code block)
    pc: usize,
    // Print debug information when running
    debug: bool,
    // Single global symbol table for all method names (matches wren_c)
    // Separate Struct to allow easier passing to register_primitive
    pub methods: SymbolTable,
    // FIXME: Missing pointers for wren_core.
    pub(crate) bool_class: Rc<RefCell<ObjClass>>,
    // class_class: Rc<ObjClass>,
    // fiber_class: Rc<ObjClass>,
    // fn_class: Rc<ObjClass>,
    // list_class: Rc<ObjClass>,
    // map_class: Rc<ObjClass>,
    pub(crate) null_class: Rc<RefCell<ObjClass>>,
    pub(crate) num_class: Rc<RefCell<ObjClass>>,
    // object_class: Rc<ObjClass>,
    pub(crate) range_class: Rc<RefCell<ObjClass>>,
    pub(crate) string_class: Rc<RefCell<ObjClass>>,
    pub(crate) system_class: Rc<RefCell<ObjClass>>,
}

//   PRIMITIVE(vm->numClass, "+(_)", num_plus);

pub(crate) fn wren_new_range(vm: &WrenVM, from: f64, to: f64, is_inclusive: bool) -> Rc<ObjRange> {
    Rc::new(ObjRange {
        class_obj: vm.range_class.clone(),
        from: from,
        to: to,
        is_inclusive: is_inclusive,
    })
}

impl WrenVM {
    pub fn new(debug: bool) -> Self {
        let mut vm = Self {
            module: Module::default(),
            stack: Vec::new(),
            methods: SymbolTable::default(),
            pc: 0,
            debug: debug,
            // These are all wrong.  They don't suport static methods yet.
            num_class: Rc::new(RefCell::new(ObjClass::new("Num"))),
            bool_class: Rc::new(RefCell::new(ObjClass::new("Bool"))),
            null_class: Rc::new(RefCell::new(ObjClass::new("Null"))),
            string_class: Rc::new(RefCell::new(ObjClass::new("String"))),
            range_class: Rc::new(RefCell::new(ObjClass::new("Range"))),
            system_class: Rc::new(RefCell::new(ObjClass::new("System"))),
        };

        register_core_primitives(&mut vm);
        vm
    }

    // TODO: This needs to be Option?  Root classes dont have classes?
    fn class_obj(&self, value: &Value) -> Option<Rc<RefCell<ObjClass>>> {
        match value {
            Value::Null => Some(self.null_class.clone()),
            Value::Num(_) => Some(self.num_class.clone()),
            Value::Boolean(_) => Some(self.bool_class.clone()),
            Value::String(_) => Some(self.string_class.clone()),
            Value::Object(o) => o.class_obj(),
        }
    }

    pub fn run(&mut self, closure: Closure) -> Result<(), RuntimeError> {
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
                            .class_obj(&args[0])
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
                Ops::Load(variable) => {
                    let value = match variable.scope {
                        Scope::Module => self.module.variables[variable.index].clone(),
                        Scope::Local => self.stack[variable.index].clone(),
                    };
                    self.push(value);
                }
                Ops::Store(variable) => {
                    let value = self.peek()?;
                    match variable.scope {
                        Scope::Module => self.module.variables[variable.index] = value.clone(),
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
                Ops::JumpIfFalsePlaceholder => unimplemented!(),
                Ops::JumpPlaceholder => unimplemented!(),
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
            Ops::Load(v) => format!("Load {:?} {}: {:?}", v.scope, v.index, self.stack[v.index]),
            Ops::Store(_) => format!("{:?}", op),
            Ops::JumpIfFalsePlaceholder => format!("{:?}", op),
            Ops::JumpIfFalse(_) => format!("{:?}", op),
            Ops::JumpPlaceholder => format!("{:?}", op),
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
            Ops::Jump(_) => format!("{:?}", self),
            Ops::Loop(_) => format!("{:?}", self),
            Ops::Pop => format!("{:?}", self),
            Ops::End => format!("{:?}", self),
        }
    }
}

pub fn debug_bytecode(_vm: &WrenVM, closure: &Closure) {
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
    fn class_obj(&self) -> Option<Rc<RefCell<ObjClass>>>;
}

#[derive(Debug)]
pub(crate) struct ObjRange {
    class_obj: Rc<RefCell<ObjClass>>,
    // The beginning of the range.
    from: f64,
    // The end of the range. May be greater or less than [from].
    to: f64,
    // True if [to] is included in the range.
    is_inclusive: bool,
}

impl Obj for ObjRange {
    fn obj_type(&self) -> ObjType {
        ObjType::Range
    }
    fn class_obj(&self) -> Option<Rc<RefCell<ObjClass>>> {
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

#[derive(Debug)]
pub struct ObjClass {
    //   ObjClass* superclass;

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
    name: String, // Should be Rc<ObjString>

                  // The ClassAttribute for the class, if any
                  //   Value attributes;
}

impl ObjClass {
    fn new(name: &str) -> ObjClass {
        ObjClass {
            name: name.to_string(),
            methods: Vec::new(),
        }
    }

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

    fn class_obj(&self) -> Option<Rc<RefCell<ObjClass>>> {
        None
    }
}

pub(crate) fn register_primitive(
    symbols: &mut SymbolTable,
    class: &mut ObjClass,
    name: &str,
    primitive: Primitive,
) {
    let index = symbols.ensure_method(name);
    class.set_method(index, Method::Primitive(primitive));
}
