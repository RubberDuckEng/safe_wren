# wren_rust
 Playing around with writing a wren compiler in rust.

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

## Ordered goals?
* Investigate break/nested_for_loop.wren, it may indicate a discard_locals problem.
* Time the tests / make faster
* Forward-declared module variables. (wren_c has this magic where all module.variables are either classes, null or numbers during compile time.  Numbers = line numbers meaning "this hasn't been declared", null = declared, and classes are declared and set to that class via class declarations.) (blocks wren_core.wren)
* Upvalues
* String codepoint APIs (including String.iterate)
* Map.iterate
* foreign functions
* remove stub_core from core initialization.
* Give different types to Symbol, Constant, etc.
* attributes
* Full wren_core.wren parsing
* Limit tests
* Scientific notation
* super / superclass calls
* Fiber APIs (try, etc.)
* static fields
* Raw strings
* C API
* Garbage Collection
* Sort methods to match wren_c order?

## Future bugs
* Push/Pop of scopes does not work with Result pattern.

### Leads to pursue
* Making InputManager an Iterator, could make easier the "skip until" pattern?
* https://docs.rs/once_cell/1.8.0/once_cell/
* https://docs.rs/anyhow/1.0.41/anyhow/

### wren_c bugs
* closures/functions defined in wren_core.wren end up with a null class pointer?
* If you yield from the root, it gets set to state=OTHER, presumably later you
might be able to call things on it?