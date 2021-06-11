use std::env;

#[macro_use]
extern crate num_derive;
extern crate num_traits;

mod vm;

use crate::vm::*;

fn main() {
    let args: Vec<_> = env::args().collect();
    if args.len() < 2 {
        println!("Usage: `cargo run STATEMENT`");
        println!("Example: `cargo run \"1+1\"`");
    } else {
        let input = InputManager::from_string("1 + 1");
        // let token = next_token(&mut input);
        // let tokens = lex(&mut input);
        let closure = compile(input);
        println!("{:?}", closure);
        let mut vm = WrenVM::new();
        vm.run(closure.expect("foo")).expect("VM Error");
        println!("{:?}", vm.stack);
        // println!("Executing: {}", args[1]);
        // println!("{:?}", parse(&args[1])));
    }
}
