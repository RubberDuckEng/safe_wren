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


## Ordered goals?
* Multiple Compiler objects
* function parsing
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


### Leads to pursue
* Making InputManager an Iterator, could make easier the "skip until" pattern?
* https://docs.rs/once_cell/1.8.0/once_cell/
* https://docs.rs/anyhow/1.0.41/anyhow/