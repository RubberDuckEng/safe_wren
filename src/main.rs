use std::env;
use std::fs;

#[macro_use]
extern crate num_derive;
extern crate num_traits;

mod compiler;
mod vm;

use crate::compiler::{compile, lex, InputManager};
use crate::vm::WrenVM;

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
    let source = fs::read_to_string(path).unwrap_or_else(|_| {
        eprintln!("Could not find file \"{}\".", path);
        exit(ExitCode::NoInput);
    });
    // handle module setup.
    let mut vm = WrenVM::new();
    let input = InputManager::from_string(source);
    let module_name = "dummy_module";
    let closure = compile(&mut vm, input, module_name).unwrap_or_else(|e| {
        println!("{:?}", e);
        exit(ExitCode::CompileError);
    });

    vm.run(closure).unwrap_or_else(|e| {
        println!("{:?}", e);
        exit(ExitCode::RuntimeError);
    });
}

fn wren_test_main(args: &Vec<String>) {
    handle_usage(&args);
    // handle API tests.
    run_file(&args[1]);
}

fn print_tokens(source: &String) {
    let input = InputManager::from_string(source.clone());
    let tokens = lex(input);
    println!("{:?}", tokens);
}

fn print_bytecode(source: &String) {
    let mut vm = WrenVM::new();
    let input = InputManager::from_string(source.clone());
    let closure = compile(&mut vm, input, "dummy_module");
    println!("{:?}", closure);
}

fn interpret_and_print_vm(source: &String) {
    let mut vm = WrenVM::new();
    let input = InputManager::from_string(source.clone());
    let closure = compile(&mut vm, input, "dummy_module").expect("compile");
    vm.run(closure).expect("runtime");
    println!("{:?}", vm);
}

fn main() {
    let args: Vec<_> = env::args().collect();
    if args[1] == "--tokenize" {
        print_tokens(&args[2]);
    } else if args[1] == "--compile" {
        print_bytecode(&args[2]);
    } else if args[1] == "--interpret" {
        interpret_and_print_vm(&args[2]);
    } else {
        wren_test_main(&args);
    }
    exit(ExitCode::Success);
}
