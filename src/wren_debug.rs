// For debugging safe_wren.  Uses APIs which are not public.

use crate::compiler::{lex, InputManager, WrenError};
use crate::test::test_config;
use crate::vm::{debug_bytecode, RuntimeError, VM};
use crate::wren::DebugLevel;

use vmgc::heap::*;

fn print_compile_error(e: WrenError) {
    // Matching test.c output:
    eprintln!("[{} line {}] {}", e.module, e.line, e.error);
}

// wren_test uses:
// Index must be a number.
// [.test/core/string.../iterator_value_not_num line 1] in (script)
fn print_runtime_error(e: RuntimeError) {
    eprintln!("{}", e.msg);
    for frame in &e.stack_trace.frames {
        eprintln!(
            "[{} line {}] in {}",
            frame.module, frame.line, frame.fn_name
        );
    }
}

pub fn print_tokens(bytes: Vec<u8>) {
    let mut input = InputManager::from_bytes(bytes);
    let result = lex(&mut input);

    if let Ok(tokens) = result {
        let mut as_string = Vec::new();
        for token in tokens {
            as_string.push(format!("{:?} '{}'", token.token, token.name(&input)));
        }
        println!("Tokens: [{}]", as_string.join(", "));
    } else {
        println!("{:?}", result);
    }
}

pub fn print_bytecode(bytes: Vec<u8>, module_name: String) {
    let input = InputManager::from_bytes(bytes);
    let mut vm = VM::new(test_config());
    let scope = HandleScope::new(&vm.heap);
    let result = vm.compile_in_module(&scope, &module_name, input);
    match result {
        Ok(closure) => debug_bytecode(&vm, closure.as_ref()),
        Err(e) => print_compile_error(e),
    }
}

pub fn interpret_and_print_vm(bytes: Vec<u8>, module_name: String) {
    let input = InputManager::from_bytes(bytes);
    let mut vm = VM::new(test_config());
    vm.config.debug_level = Some(DebugLevel::NonCore);
    let scope = HandleScope::new(&vm.heap);
    let result = vm.compile_in_module(&scope, &module_name, input);
    match result {
        Ok(closure) => match vm.run(closure) {
            Ok(_) => println!("{:?}", vm),
            Err(e) => print_runtime_error(e),
        },
        Err(e) => print_compile_error(e),
    }
}
