// Uses APIs which are not public, eventually to be split from
// wren_test (which only uses public APIs).

use std::fs;

use crate::compiler::{compile_in_module, lex, InputManager, WrenError};
use crate::test::test_config;
use crate::vm::{wren_debug_bytecode, RuntimeError, WrenVM};
use crate::wren::DebugLevel;

fn print_compile_error(e: WrenError) {
    // Matching test.c output:
    eprintln!("[{} line {}] {}", e.module, e.line, e.error);
}

// wren_test expects:
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

struct CompileInput {
    input: InputManager,
    module_name: String,
}

fn input_from_source_or_path(source_or_path: &str) -> CompileInput {
    if source_or_path.ends_with(".wren") {
        let source = fs::read_to_string(source_or_path).unwrap_or_else(|_| {
            eprintln!("Could not find file \"{}\".", source_or_path);
            std::process::exit(-1);
        });
        CompileInput {
            input: InputManager::from_string(source),
            module_name: source_or_path.strip_suffix(".wren").unwrap().into(),
        }
    } else {
        CompileInput {
            input: InputManager::from_string(source_or_path.to_string()),
            module_name: "<inline>".into(),
        }
    }
}

pub fn print_tokens(source_or_path: &str) {
    let mut input = input_from_source_or_path(source_or_path);
    let result = lex(&mut input.input);

    if let Ok(tokens) = result {
        let mut as_string = Vec::new();
        for token in tokens {
            as_string.push(format!("{:?} '{}'", token.token, token.name(&input.input)));
        }
        println!("Tokens: [{}]", as_string.join(", "));
    } else {
        println!("{:?}", result);
    }
}

pub fn print_bytecode(source_or_path: &str) {
    let mut vm = WrenVM::new(test_config());
    let input = input_from_source_or_path(source_or_path);
    let result = compile_in_module(&mut vm, &input.module_name, input.input);
    match result {
        Ok(closure) => wren_debug_bytecode(&vm, &closure.borrow()),
        Err(e) => print_compile_error(e),
    }
}

pub fn interpret_and_print_vm(source_or_path: &str) {
    let mut vm = WrenVM::new(test_config());
    vm.config.debug_level = Some(DebugLevel::NonCore);
    let input = input_from_source_or_path(source_or_path);
    let result = compile_in_module(&mut vm, &input.module_name, input.input);
    match result {
        Ok(closure) => match vm.run(closure) {
            Ok(_) => println!("{:?}", vm),
            Err(e) => print_runtime_error(e),
        },
        Err(e) => print_compile_error(e),
    }
}
