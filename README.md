# wren_rust
An (incomplete) implementation of Wren (wren.io) in Rust

## Differences from wren_c
* reference-counted, no garbage collected.
* expects utf8 input
* Strings are always utf8 (do not allow invalid utf8 bytes)
* does not allow overriding allocator (yet?)

## Usage

There are two binaries:
- wren_test -- used for testing uses only public API
- wren_debug -- used for debugging vm, uses private calls.

`cargo run FILENAME_OR_STRING`
will run wren_test against a file or string.


`cargo run --bin=wren_debug FILENAME_OR_STRING` will run wren_debug

wren_debug sub-commands:
`tokenize` Dumps token stream.
`compile`  Dumps compiler bytecode.
`interpret` Similar to no arguments, excepts prints VM state after run.


`python3 util/test.py` will run the tests, including updating `test_results/*`
with error text from any failed tests.  `test.py` will also update
`test_results/passes.txt` with the list of passing tests.

`test_results/test_expectations.txt` lists all currently skipped tests and why.

## Ordered goals?
* Time the tests / make faster
* Upvalues
* remove all uses of 'as' (use into() instead).
* validateSuperclass could now use ClassSource to validate internal, etc.
* String codepoint APIs (including String.iterate)
* wrong line numbers for foreign method runtime errors.
* Give different types to Symbol, Constant, etc.
* attributes
* Scientific notation
* static fields (emits to two compilers)
* C API
* Garbage Collection?
* Sort methods to match wren_c order?

## Future bugs
* Push/Pop of scopes does not work with Result pattern.

### Leads to pursue
* Making InputManager an Iterator, could make easier the "skip until" pattern?
* https://docs.rs/once_cell/1.8.0/once_cell/
* https://docs.rs/anyhow/1.0.41/anyhow/
* Try optimizing for size: https://github.com/johnthagen/min-sized-rust
* To try for really-small size, no-std + https://doc.rust-lang.org/alloc/
* Box has the same layout as a C pointer, possibly simplifying our FFI?
https://stackoverflow.com/questions/62338832/how-to-hold-rust-objects-in-rust-code-created-through-c
https://doc.rust-lang.org/nomicon/ffi.html#representing-opaque-structs seems to imply so?

### wren_c bugs
* closures/functions defined in wren_core.wren end up with a null class pointer?
* If you yield from the root, it gets set to state=OTHER, presumably later you
might be able to call things on it?
* WrenConfiguration likely leaked for each WrenVM constructed/destructed?