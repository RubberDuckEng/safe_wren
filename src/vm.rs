use std::rc::Rc;
use std::str;

use crate::compiler::{Ops, Scope};

#[derive(Debug, Clone)]
pub enum Value {
    Null,
    Num(f64),
    Boolean(bool),
    String(Rc<String>),
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
}

impl Value {
    fn try_into_num(self) -> Result<f64, RuntimeError> {
        match self {
            Value::Num(value) => Ok(value),
            _ => Err(RuntimeError::NumberRequired(self)),
        }
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
    pub method_names: Vec<String>,
    // FIXME: Missing pointers for wren_core.
}

// enum Method {
//     Primitive,
//     ForeignFunction,
//     Closure,
// }

//   PRIMITIVE(vm->numClass, "+(_)", num_plus);

// System.print is not actually in C in wren_c, but since we can't yet parse
// classes or methods, implementing here to get unit tests working.
fn prim_system_print(value: Value) -> Value {
    let string = match &value {
        Value::Null => "null".into(),
        Value::Num(i) => format!("{}", i),
        Value::Boolean(b) => format!("{}", b),
        Value::String(s) => format!("{}", s),
    };

    println!("{}", string);
    value
}

impl WrenVM {
    pub fn new(debug: bool) -> Self {
        Self {
            module: Module::default(),
            stack: Vec::new(),
            method_names: Vec::new(),
            pc: 0,
            debug: debug,
        }
    }

    pub fn ensure_method_symbol(&mut self, name: &str) -> usize {
        if let Some(index) = self.method_names.iter().position(|n| n.eq(name)) {
            return index;
        }

        // New symbol, so add it.
        self.method_names.push(name.into());
        self.method_names.len() - 1
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
                    if signature.full_name.eq("+(_)") {
                        let other = self.pop()?.try_into_num()?;
                        let this = self.pop()?.try_into_num()?;
                        self.push(Value::Num(this + other));
                    } else if signature.full_name.eq("-(_)") {
                        let other = self.pop()?.try_into_num()?;
                        let this = self.pop()?.try_into_num()?;
                        self.push(Value::Num(this - other));
                    } else if signature.full_name.eq("-") {
                        let this = self.pop()?.try_into_num()?;
                        self.push(Value::Num(-this));
                    } else if signature.full_name.eq("*(_)") {
                        let other = self.pop()?.try_into_num()?;
                        let this = self.pop()?.try_into_num()?;
                        self.push(Value::Num(this * other));
                    } else if signature.full_name.eq("/(_)") {
                        let other = self.pop()?.try_into_num()?;
                        let this = self.pop()?.try_into_num()?;
                        self.push(Value::Num(this / other));
                    } else if signature.full_name.eq("<(_)") {
                        let other = self.pop()?.try_into_num()?;
                        let this = self.pop()?.try_into_num()?;
                        self.push(Value::Boolean(this < other));
                    } else if signature.full_name.eq(">(_)") {
                        let other = self.pop()?.try_into_num()?;
                        let this = self.pop()?.try_into_num()?;
                        self.push(Value::Boolean(this > other));
                    } else if signature.full_name.eq("print(_)") {
                        let value = self.pop()?;
                        self.pop()?; // this value.
                        self.push(prim_system_print(value));
                    } else {
                        return Err(RuntimeError::MethodNotFound(signature.full_name.clone()));
                    }
                    // Get symbol # from signature?
                    // Args are on the stack.  Grab a slice?
                    // Look up the class for the first arg.
                    // If the class's method table doesn't include the symbol, bail.
                    // method = &classObj->methods.data[symbol]
                    // match on method type.
                    // If primative, make direct call.  Expecting result on the stack.
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
