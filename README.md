# safe_wren
A nearly-complete implementation of the Wren language (wren.io) in Rust.

The original https://github.com/wren-lang/wren from wren.io is
refered to as `wren_c` to disambiguate from this `safe_wren`.

This should be considered a alpha release.  safe_wren has only been used
locally for test cases and command line, we've not yet seen it integrated
with larger programs.  Feedback welcome: https://github.com/RubberDuckEng/safe_wren/issues

## Similaries to wren_c
* Passes ~90% of wren_c tests
* Exposes ~90% of wren_c C API

## Differences from wren_c
* Never crashes to bad input (but can currently still be timed-out via infinite loops)
* Reference-counted, not (yet) garbage collected (in progress).
* Stops at first compile error (instead of continuing)
* Requires utf8 input and wren `String`s are strings of utf8, not byte-buffers (safe_wren does not allow invalid utf8 bytes)
* Does not allow overriding allocator (yet)
* Still missing opt-meta, and class attributes
* Currently about 2x slower than wren_c on some microbenchmarks (should be comparable after GC work completes)

## Usage

### From an existing C project:

`cargo build --release` produces `target/release/libsafe_wren.a` and
`target/release/libsafe_wren.so` which are drop-in replacements for
`wren.a` and `wren.so` and compatible with `wren.h`
found at (`wren_c/src/include/wren.h`).


### From Rust:
Add a dependency to your `cargo.toml`, e.g.

```
[dependencies]
safe_wren = "0.1.0"
```

## Development

Two binaries are provided for development:
- `wren_test` -- used for testing, uses only public API
- `wren_debug` -- used for debugging vm, uses private calls.

`cargo run FILENAME_OR_STRING` will run `wren_test` against a file or string.


`cargo run --bin=wren_debug COMMAND FILENAME_OR_STRING` will run `wren_debug`

`wren_debug` commands:
* `tokenize` Dumps token stream.
* `compile`  Dumps compiler bytecode.
* `interpret` Similar to `wren_test`, excepts prints VM state after run.


`python3 util/test.py` will run the tests, including updating `test_results/*`
with error text from any failed tests.  `test.py` will also update
`test_results/passes.txt` with the list of passing tests.

`python3 util/to_fix.py` will summarize `test_results/*`.

`test_results/test_expectations.txt` lists all currently skipped tests and why.

`python3 utils/compare.py` will compare output for wren_test from safe_wren vs. wren_c.

`cargo fuzz` works, follow instructions at https://rust-fuzz.github.io/book/cargo-fuzz.html
will require using nightly rust toolchains as of Oct 2021.  I haven't seen crashes in months
mostly it finds timeouts due to lack of a timeout mechanism in safe_wren (yet).

@eseidel's normal development loop:
1. Figure out what the next issue to tackle (this `README.md` or `test_results/test_expectations.txt`).
2. Implement it, fight the rust compiler until it compiles.
3. Write a small test in `tests/bringup`.
4. Use `cargo run` on the test, if fails go back to debugging.
5. Use `cargo run --bin=wren_debug interpret` to see what interpreter is doing.
6. Use `cargo build && python3 util/test.py; python3 util/to_fix.py` to run all tests and update `common_test_errors.txt`.
7. Repeat.

## Work yet to do

### Launch list?
* Example using rust API
* Example using C API
* Announce to wren-lang
* Generate and publish Rust docs.

### Ordered goals?
* Garbage Collection, in progress: https://github.com/RubberDuckEng/vmgc
* fix local_outside_method.wren
* Time the tests / make faster (next is vec::alloc from method calls)
* Fancier test_expectations system
 ** Config / Expectation pairs (c | FAIL, RUST | TIMEOUT)
* Teach `utils/test.py` how to easily switch between rust and c_rust and c
* remove all uses of 'as' (use into() instead).
* validate_superclass could now use ClassSource to validate internal, etc.
* String codepoint APIs (including String.iterate)
* wrong line numbers for foreign method runtime errors.
* Class attributes: https://wren.io/classes.html#attributes
* rust implementation of meta package.
* continue after failure during compiling?
* \x should not round-trip through char.
* Sort methods to match wren_c order?
* Variable should use a different type for each scope type.
* fuzz both wren_c and safe_wren and compare output?
* Add a timeout mechanism for slow/infinite loops/scripts.
* Look at some of the slow-unit fuzz results
 ** fuzz/artifacts/fuzz_target_1/slow-unit-63ea01d2d5ba869bdb889c3b51b21350d5a4ffea (lookup_symbol should be a hash)
 ** fuzz/artifacts/fuzz_target_1/slow-unit-355b25c3fc10bfd14a363cf737abf3a07bab4a1e (needless stack resizing)
* wren_debug interpret wren_c/test/language/static_field/nested_class.wren does out of bound lookup in line_for_pc.

### Leads to pursue
* Making InputManager an Iterator, could make easier the "skip until" pattern?
  e.g. https://doc.rust-lang.org/stable/std/iter/struct.Peekable.html
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


## GC Conversion
* ObjRange eq and hash.