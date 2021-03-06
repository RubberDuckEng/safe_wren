#![no_main]
use libfuzzer_sys::fuzz_target;

extern crate safe_wren;

use safe_wren::*;

fuzz_target!(|data: &[u8]| {
    if let Ok(source) = std::str::from_utf8(data) {
        let config = Configuration::default();
        let mut vm = VM::new(config);
        vm.interpret("module", source.to_string());
    }
});
