// analog to main.c from wren_test in wren_c.
// wren_rust's version of wren_test.  Should only use public APIs
// Currently has additional --tokenize, --compile and --interpret
// options, which do use private interfaces and should be split out
// into a separate executable.

use std::env;
use std::fs;

#[macro_use]
extern crate num_derive;
extern crate num_traits;

mod compiler;
mod core;
mod vm;
mod wren;

mod test;
mod wren_debug;

use crate::test::test_config;
use crate::wren_debug::{interpret_and_print_vm, print_bytecode, print_tokens};

use crate::wren::*;

enum ExitCode {
    Success = 0,
    GenericError = 1,
    Usage = 64,
    CompileError = 65,
    RuntimeError = 70,
    // wren_c wren_test has NoInput, but always exits IO_ERR instead.
    NoInput = 66,
    // IOError = 74,
}

fn handle_usage(args: &Vec<String>) {
    if args.len() < 2 {
        println!("This is a Wren test runner.\nUsage: wren_test [file]\n");
        exit(ExitCode::Usage);
    }

    if args.len() == 2 && args[1] == "--version" {
        println!(
            "wren_test is running on Wren version {}\n",
            WREN_VERSION_STRING
        );
        exit(ExitCode::GenericError);
    }
}

fn exit(code: ExitCode) -> ! {
    use std::process;
    process::exit(code as i32);
}

fn run_file(path: &String) -> ! {
    let source = fs::read_to_string(path).unwrap_or_else(|e| {
        eprintln!("Failed to open file \"{}\": {}", path, e);
        // FIXME: wren_c appears to read the file as bytes and
        // does not exit NO_INPUT for invalid UTF-8, but rather
        // expects invalid UTF-8 to be a compile error.
        // Unclear if that's worth repliciating?
        exit(ExitCode::NoInput);
    });

    // handle module setup.
    let mut vm = WrenVM::new(test_config());
    let module_name = path.strip_suffix(".wren").unwrap();
    let result = wren_interpret(&mut vm, module_name, source);
    match result {
        WrenInterpretResult::CompileError => {
            exit(ExitCode::CompileError);
        }
        WrenInterpretResult::RuntimeError => {
            exit(ExitCode::RuntimeError);
        }
        WrenInterpretResult::Success => {
            exit(ExitCode::Success);
        }
    }
}

fn wren_test_main(args: &Vec<String>) {
    handle_usage(&args);
    // handle API tests.
    run_file(&args[1]);
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
