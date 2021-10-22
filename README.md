# safe_wren
A mostly-complete implementation of Wren (wren.io) in Rust.

## Similaries to wren_c
* Passes ~90% of wren_c tests
* Exposes ~90% of wren_c C API

## Differences from wren_c
* Never crashes to bad input (but can currently still be timed-out via infinite loops)
* Reference-counted, no garbage collected (in progress).
* Stops at first compile error (instead of continuing)
* Requires utf8 input and strings are always utf8 (does not allow invalid utf8 bytes)
* Does not allow overriding allocator (yet)
* Missing opt-meta, and class attributes
* About 2x slower than wren_c on some microbenchmarks (should be compariable after GC work completes)

## Usage

### From an existing C project:

`cargo build --release` produces libsafe_wren.a and libsafe_wren.so
which are drop-in replacements for wren.a and wren.so and compatible
with wren.h found at (wren_c/src/include/wren.h).


### From Rust:
Add a dependency to your cargo.toml, e.g.

```
[dependencies]
safe_wren = "0.1.0"
```

## Development

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

## Work yet to do

### Launch list?
* Example using rust API
* Example using C API
* Publish to Cargo
* Announce to wren-lang

### Ordered goals?
* Fix delta blue (closure error!)
* fix local_outside_method.wren
* Time the tests / make faster (next is vec::alloc from method calls)
* Fancier test_expectations system
 ** Config / Expectation pairs (c | FAIL, RUST | TIMEOUT)
* Teach test.py how to easily switch between rust and c_rust and c
* remove all uses of 'as' (use into() instead).
* validate_superclass could now use ClassSource to validate internal, etc.
* String codepoint APIs (including String.iterate)
* wrong line numbers for foreign method runtime errors.
* attributes
* rust implementation of meta package.
* continue after failure during compiling?
* \x should not round-trip through char.
* Garbage Collection?
* Sort methods to match wren_c order?
* Variable should use a different type for each scope type.
* fuzz both wren_c and safe_wren and compare output?
* Look at some of the slow-unit fuzz results
 ** fuzz/artifacts/fuzz_target_1/slow-unit-63ea01d2d5ba869bdb889c3b51b21350d5a4ffea (lookup_symbol should be a hash)
 ** fuzz/artifacts/fuzz_target_1/slow-unit-355b25c3fc10bfd14a363cf737abf3a07bab4a1e (needless stack resizing)
* wren_debug interpret wren_c/test/language/static_field/nested_class.wren does out of bound lookup in line_for_pc.

### Leads to pursue
* Making InputManager an Iterator, could make easier the "skip until" pattern?
* https://docs.rs/once_cell/1.8.0/once_cell/
* https://docs.rs/anyhow/1.0.41/anyhow/
* Try optimizing for size: https://github.com/johnthagen/min-sized-rust
* To try for really-small size, no-std + https://doc.rust-lang.org/alloc/
* Box has the same layout as a C pointer, possibly simplifying our FFI?
https://stackoverflow.com/questions/62338832/how-to-hold-rust-objects-in-rust-code-created-through-c
https://doc.rust-lang.org/nomicon/ffi.html#representing-opaque-structs seems to imply so?

### Benchmarking notes
* Current numbers show safe_wren to be about 2.5x-9x slower than wren_c across
  the various microbenchmarks.  Unclear what real-world effect this would have.
* Value::clone is apparent in many benchmarks.
* Using move/drain semantics when calling args from the stack could help avoid
  needing to clone values when converting them with try_into_X.
  Or at least make try_into_X use move semenatics and the clone explicit in
  the caller.
* class_for_value under call_method shows up at ~5% on several benchmarks.
* map_numeric heavily tests Value::PartialEq
* binary_trees leans heavily (at least 20% of time) on ptr::drop_in_place
  (deallocation) of RefCell<ObjInstance>.  GC would reduce this.
* map_string spends 57% of time in core::string_plus, and 20% of time
  truncating the stack.

## wren_c bugs
* closures/functions defined in wren_core.wren end up with a null class pointer?
* If you yield from the root, it gets set to state=OTHER, presumably later you
might be able to call things on it?
* WrenConfiguration likely leaked for each WrenVM constructed/destructed?
