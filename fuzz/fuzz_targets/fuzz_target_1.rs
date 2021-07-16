#![no_main]
use libfuzzer_sys::fuzz_target;

extern crate wren_rust;

use wren_rust::*;

fuzz_target!(|data: &[u8]| {
    if let Ok(source) = std::str::from_utf8(data) {
        let config = WrenConfiguration::default();
        let mut vm = WrenVM::new(config);
        wren_interpret(&mut vm, "module", source.to_string());
    }
});
