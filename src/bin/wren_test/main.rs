// analog to main.c from wren_test in wren_c.
// safe_wren's version of wren_test.  Should only use public APIs
// Currently has additional --tokenize, --compile and --interpret
// options, which do use private interfaces and should be split out
// into a separate executable.

use std::env;
use std::fs;

extern crate safe_wren;

mod api_tests;
mod error;

use api_tests::api_test_bind_foreign_method_fn;
use safe_wren::test::test_config;
use safe_wren::wren::*;
use vmgc::heap::*;

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
        println!("wren_test is running on Wren version {}\n", VERSION_STRING);
        exit(ExitCode::GenericError);
    }
}

fn exit(code: ExitCode) -> ! {
    use std::process;
    process::exit(code as i32);
}

fn run_file(scope: &HandleScope, vm: &mut VM, path: &str) -> ! {
    let source = fs::read(path).unwrap_or_else(|e| {
        eprintln!("Failed to open file \"{}\": {}", path, e);
        exit(ExitCode::NoInput);
    });

    // handle module setup.
    let mut module_name = match path.strip_suffix(".wren") {
        Some(stripped) => stripped,
        // FIXME: Not sure if other parts of the code assume paths end in wren?
        None => path,
    }
    .to_string();
    if !module_name.starts_with(".") {
        module_name = format!("./{}", module_name);
    }
    match vm.interpret_bytes(scope, &module_name, source) {
        InterpretResult::CompileError => exit(ExitCode::CompileError),
        InterpretResult::RuntimeError => exit(ExitCode::RuntimeError),
        InterpretResult::Success => exit(ExitCode::Success),
    }
}

fn is_api_test_path(test_path: &str) -> bool {
    test_path.contains("test/api") || test_path.contains("test/benchmark")
}

fn main() {
    let args: Vec<_> = env::args().collect();
    handle_usage(&args);
    let test_path = &args[1];
    let api_test = is_api_test_path(test_path);
    let mut config = test_config();
    if api_test {
        config.bind_foreign_method_fn = Some(api_test_bind_foreign_method_fn);
    }
    let heap = Heap::new(100000).unwrap();
    let scope = HandleScope::new(&heap);
    let mut vm = VM::new(&scope, config);

    // handle API tests.
    run_file(&scope, &mut vm, test_path);
}
