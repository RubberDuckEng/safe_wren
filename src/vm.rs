use std::str;

use crate::compiler::Ops;

#[derive(Debug, Copy, Clone)]
pub enum Value {
    Num(u64),
}

#[derive(Default, Debug)]
pub struct Module {
    // Missing some pointer to the actual code?

    // Should this just be a map?  wren_utils.h suggests so?
    variables: Vec<Value>,
    variable_names: Vec<String>,
    // name: String, // Should be a GC'd object?
}

impl Module {
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
    MethodNotFound,
}

impl Value {
    #![allow(unreachable_patterns)] // Temporary until we have more Value types.
    fn try_into_num(self) -> Result<u64, RuntimeError> {
        match self {
            Value::Num(value) => Ok(value),
            _ => Err(RuntimeError::NumberRequired(self)),
        }
    }
}

#[derive(Debug)]
pub struct WrenVM {
    pub module: Module, // No support for multiple modules yet.
    pub stack: Vec<Value>,
    pc: usize,
    // Missing pointers for wren_core.
    // Missing Global Symbol Table.
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
    let string = match value {
        Value::Num(i) => format!("{}", i),
    };

    println!("{}", string);
    value
}

impl WrenVM {
    pub fn new() -> Self {
        Self {
            module: Module::default(),
            stack: Vec::new(),
            pc: 0,
        }
    }

    pub fn run(&mut self, closure: Closure) -> Result<(), RuntimeError> {
        loop {
            let op = &closure.function.code[self.pc];
            self.pc += 1;
            match op {
                Ops::Constant(index) => {
                    self.push(closure.function.constants[*index]);
                }
                Ops::Call(signature) => {
                    if signature.name.eq("+") {
                        let a = self.pop()?.try_into_num()?;
                        let b = self.pop()?.try_into_num()?;
                        self.push(Value::Num(a + b));
                    } else if signature.name.eq("print") {
                        let value = self.pop()?;
                        self.pop()?; // this value.
                        self.push(prim_system_print(value));
                    } else {
                        return Err(RuntimeError::MethodNotFound);
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
                    let value = self.module.variables[variable.index];
                    self.push(value);
                }
                Ops::Pop => {
                    self.pop()?;
                }
                Ops::End => {
                    return Ok(());
                }
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
}
