# wren_rust
 Playing around with writing a wren compiler in rust.

## Usage

`cargo run FILENAME_OR_STRING`

Options:
`--tokenize` Dumps token stream.
`--compile`  Dumps compiler bytecode.
`--interpret` Similar to no arguments, excepts prints VM state after run.

`python3 util/test.py` will run the tests, including updating `test_results/*`
with error text from any failed tests.  `test.py` will also update
`test_results/passes.txt` with the list of passing tests.

## Ideas
* use Drop for auto-scope
* Split Value into Value<Object> and TypedObject which can be returned from Obj trait.
* Pull the module off the VM and hand it to the parser during parsing.
* pull the symbol table off the vm durign compile?

## Ordered goals?
* Investigate break/nested_for_loop.wren, it may indicate a discard_locals problem.
* Time the tests / make faster
* String interpolation (blocks wren_core.wren)
* Forward-declared module variables. (wren_c has this magic where all module.variables are either classes, null or numbers during compile time.  Numbers = line numbers meaning "this hasn't been declared", null = declared, and classes are declared and set to that class via class declarations.) (blocks wren_core.wren)
* Upvalues
* String codepoint APIs (including String.iterate)
* Map.iterate
* remove all f64 casting (hide behind methods)
* foreign functions
* attributes
* Full wren_core.wren parsing
* Limit tests
* Scientific notation
* super / superclass calls
* Fiber APIs (try, etc.)
* imports
* static fields
* Raw strings
* C API
* Garbage Collection
* System.clock
* Sort methods to match wren_c order?
* What happens if you pass too many values to a function?

## Most failing tests
* Maps/map lookup, but requires object allocation
* "construct" keyword, missing function declarations.

## Future bugs
* Push/Pop of scopes does not work with Result pattern.
* Fn is defined twice once in code and once in wren_core.wren
* Object/Class are not set as global variables?
* call_runtime_error is seeing Fn as this rather than Bool, why?

### Leads to pursue
* Making InputManager an Iterator, could make easier the "skip until" pattern?
* https://docs.rs/once_cell/1.8.0/once_cell/
* https://docs.rs/anyhow/1.0.41/anyhow/

### wren_c bugs
* closures/functions defined in wren_core.wren end up with a null class pointer?