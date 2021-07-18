// analog to main.c from wren_test in wren_c.
// wren_rust's version of wren_test.  Should only use public APIs
// Currently has additional --tokenize, --compile and --interpret
// options, which do use private interfaces and should be split out
// into a separate executable.

use std::env;

extern crate wren_rust;

use wren_rust::wren_debug::{interpret_and_print_vm, print_bytecode, print_tokens};

fn print_usage() {
    println!("Usage:");
    println!("wren_debug MODE PATH_OR_STRING");
    println!("");
    println!("MODE        DESCRIPTION");
    println!("tokenize -> print token stream");
    println!("compile  -> print bytecode stream");
    println!("interpet -> interpet with debugging enabled");
}

fn main() {
    let args: Vec<_> = env::args().collect();
    if args.len() < 2 {
        print_usage();
        return;
    }
    let command = &args[1];
    if command.eq("tokenize") {
        print_tokens(&args[2]);
    } else if command.eq("compile") {
        print_bytecode(&args[2]);
    } else if command.eq("interpret") {
        interpret_and_print_vm(&args[2]);
    } else {
        print_usage();
    }
}
