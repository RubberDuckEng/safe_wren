// analog to main.c from wren_test in wren_c.
// wren_rust's version of wren_test.  Should only use public APIs
// Currently has additional --tokenize, --compile and --interpret
// options, which do use private interfaces and should be split out
// into a separate executable.

use std::env;
use std::fs;

extern crate wren_rust;

use wren_rust::test::test_config;
use wren_rust::wren::*;

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
        // expects invalid UTF-8 to appear as compile errors
        // including the line number they occured on.
        // We can't pass those with this up-front decoding strategy.
        exit(ExitCode::NoInput);
    });

    // handle module setup.
    let mut vm = WrenVM::new(test_config());
    let mut module_name = match path.strip_suffix(".wren") {
        Some(stripped) => stripped,
        // FIXME: Not sure if other parts of the code assume paths end in wren?
        None => path,
    }
    .to_string();
    if !module_name.starts_with(".") {
        module_name = format!("./{}", module_name);
    }
    let result = wren_interpret(&mut vm, &module_name, source);
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

fn main() {
    let args: Vec<_> = env::args().collect();
    handle_usage(&args);
    // handle API tests.
    run_file(&args[1]);
}
