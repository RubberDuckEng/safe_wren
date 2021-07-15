// analog to main.c from wren_test in wren_c.
// wren_rust's version of wren_test.  Should only use public APIs
// Currently has additional --tokenize, --compile and --interpret
// options, which do use private interfaces and should be split out
// into a separate executable.

use std::env;

extern crate wren_rust;

use wren_rust::wren_debug::{interpret_and_print_vm, print_bytecode, print_tokens};

fn main() {
    let args: Vec<_> = env::args().collect();
    if args.len() > 1 && args[1] == "--tokenize" {
        print_tokens(&args[2]);
    } else if args.len() > 1 && args[1] == "--compile" {
        print_bytecode(&args[2]);
    } else if args.len() > 1 && args[1] == "--interpret" {
        interpret_and_print_vm(&args[2]);
    }
}
