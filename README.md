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
* ParseContext owns the whole stack of Compilers.  Compiler have Option<Compiler> and own their parent.
* use Drop for auto-scope
* Split Value into Value<Object> and TypedObject which can be returned from Obj trait.
* Pull the module off the VM and hand it to the parser during parsing.
* pull the symbol table off the vm durign compile?

## Ordered goals?
* Investigate break/nested_for_loop.wren, it may indicate a discard_locals problem.
* static functions
* Logical operators
* objects
* object fields
* object construction
* lists
* list literals
* maps
* map literals
* subclasses
* Upvalues
* Closures (Fn)
* foreign functions
* attributes
* Full wren_core.wren parsing
* Limit tests
* Fibers
* imports
* C API
* Garbage Collection

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