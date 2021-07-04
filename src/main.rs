use std::env;
use std::fs;

#[macro_use]
extern crate num_derive;
extern crate num_traits;

mod compiler;
mod core;
mod vm;

use crate::compiler::{compile, lex, InputManager};
use crate::vm::{wren_debug_bytecode, RuntimeError, WrenVM};

enum ExitCode {
    Success = 0,
    GenericError = 1,
    Usage = 64,
    CompileError = 65,
    RuntimeError = 70,
    NoInput = 66,
    // IOError = 74,
}

static VERSION_STRING: &str = "wren_rust-0.1";

fn handle_usage(args: &Vec<String>) {
    if args.len() < 2 {
        println!("This is a Wren test runner.\nUsage: wren_test [file]\n");
        exit(ExitCode::Usage);
    }

    if args.len() == 2 && args[1] == "--version" {
        println!("wren_test is running on Wren version {}\n", VERSION_STRING);
        exit(ExitCode::GenericError);
    }
}

fn exit(code: ExitCode) -> ! {
    use std::process;
    process::exit(code as i32);
}

fn run_file(path: &String) {
    let source = fs::read_to_string(path).unwrap_or_else(|e| {
        eprintln!("Failed to open file \"{}\": {}", path, e);
        exit(ExitCode::NoInput);
    });
    // Hack to avoid \r adjusting printed byte-ranges for ParseTokens
    // on windows:
    let no_crs = source
        .as_bytes()
        .to_vec()
        .into_iter()
        .filter(|i| *i != b'\r')
        .collect::<Vec<u8>>();

    // handle module setup.
    let mut vm = WrenVM::new(false);
    let input = InputManager::from_bytes(no_crs);
    let module_name = path.strip_suffix(".wren").unwrap();
    let closure = compile(&mut vm, input, module_name).unwrap_or_else(|e| {
        // Matching test.c output:
        eprintln!("[{} line {}] Error {:?}", e.module, e.line, e.error);
        exit(ExitCode::CompileError);
    });

    vm.run(closure).unwrap_or_else(|e| {
        dump_runtime_error(&e);
        exit(ExitCode::RuntimeError);
    });
}

// wre_test expects:
// Index must be a number.
// [.test/core/string.../iterator_value_not_num line 1] in (script)
fn dump_runtime_error(e: &RuntimeError) {
    eprintln!("{}", e.msg);
    for frame in &e.stack_trace.frames {
        eprintln!(
            "[{} line {}] in {}",
            frame.module, frame.line, frame.fn_name
        );
    }
}

fn wren_test_main(args: &Vec<String>) {
    handle_usage(&args);
    // handle API tests.
    run_file(&args[1]);
}

fn input_from_source_or_path(source_or_path: &String) -> InputManager {
    if source_or_path.ends_with(".wren") {
        let source = fs::read_to_string(source_or_path).unwrap_or_else(|_| {
            eprintln!("Could not find file \"{}\".", source_or_path);
            exit(ExitCode::NoInput);
        });
        InputManager::from_string(source)
    } else {
        InputManager::from_string(source_or_path.clone())
    }
}

fn print_tokens(source_or_path: &String) {
    let mut input = input_from_source_or_path(source_or_path);
    let result = lex(&mut input);

    if let Ok(tokens) = result {
        let mut as_string = Vec::new();
        for token in tokens {
            as_string.push(format!(
                "{:?} '{}'",
                token.token,
                token.name(&input).expect("input")
            ));
        }
        println!("Tokens: [{}]", as_string.join(", "));
    } else {
        println!("{:?}", result);
    }
}

fn print_bytecode(source_or_path: &String) {
    let mut vm = WrenVM::new(false);
    let input = input_from_source_or_path(source_or_path);
    let result = compile(&mut vm, input, "dummy_module");
    match result {
        Ok(closure) => wren_debug_bytecode(&vm, &closure.borrow()),
        Err(e) => eprintln!("{:?}", e),
    }
}

fn interpret_and_print_vm(source_or_path: &String) {
    let mut vm = WrenVM::new(true);
    let input = input_from_source_or_path(source_or_path);
    let closure = compile(&mut vm, input, "dummy_module").expect("compile");
    match vm.run(closure) {
        Ok(_) => println!("{:?}", vm),
        Err(e) => println!("{:?}", e),
    }
}

fn main() {
    let args: Vec<_> = env::args().collect();
    if args.len() > 1 && args[1] == "--tokenize" {
        print_tokens(&args[2]);
    } else if args.len() > 1 && args[1] == "--compile" {
        print_bytecode(&args[2]);
    } else if args.len() > 1 && args[1] == "--interpret" {
        interpret_and_print_vm(&args[2]);
    } else {
        wren_test_main(&args);
    }
    exit(ExitCode::Success);
}
