// analog to main.c from wren_test in wren_c.
// safe_wren's version of wren_test.  Should only use public APIs
// Currently has additional --tokenize, --compile and --interpret
// options, which do use private interfaces and should be split out
// into a separate executable.

use std::env;
use std::fs;

extern crate safe_wren;

use safe_wren::wren_debug::{interpret_and_print_vm, print_bytecode, print_tokens};

fn print_usage() {
    println!("Usage:");
    println!("wren_debug MODE PATH | --string STRING");
    println!("");
    println!("MODE        DESCRIPTION");
    println!("tokenize -> print token stream");
    println!("compile  -> print bytecode stream");
    println!("interpet -> interpet with debugging enabled");
}

struct ArgsResult {
    command: String,
    bytes: Vec<u8>,
    module_name: String,
}

fn parse_args() -> ArgsResult {
    let args: Vec<_> = env::args().collect();
    if args.len() < 2 {
        print_usage();
        std::process::exit(1);
    }
    let command = args[1].clone();
    if args[2].eq("--string") {
        ArgsResult {
            command,
            bytes: args[2].as_bytes().into(),
            module_name: "<inline>".into(),
        }
    } else {
        let path = &args[2];
        let bytes = fs::read(path).unwrap_or_else(|e| {
            eprintln!("Failed to open file \"{}\": {}", path, e);
            std::process::exit(-1);
        });
        let module_name = path.strip_suffix(".wren").unwrap_or(path).into();
        ArgsResult {
            command,
            bytes,
            module_name,
        }
    }
}

fn main() {
    let args = parse_args();
    match args.command.as_str() {
        "tokenize" => print_tokens(args.bytes),
        "compile" => print_bytecode(args.bytes, args.module_name),
        "interpret" => interpret_and_print_vm(args.bytes, args.module_name),
        _ => print_usage(),
    }
}
