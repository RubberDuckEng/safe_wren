# wren_rust
 Playing around with writing a wren compiler in rust.


## Next goals?
* make wren_c/examples all parse?
* wren_core.wren parsing?
* more tests passing?
* real function dispatch?
* if / for
* function parsing
* lists
* Logical operators
* Block comments

## Longer off
* Garbage Collection
* C API
* Closures / Upvalues
* Limit tests
* Fibers

## Missing
* Basically everything.  Only 8 of 870 tests pass.

### Passing Tests:
test/bringup/add.wren
wren_c/example/hello.wren
wren_c/test/language/comments/line_at_eof.wren
wren_c/test/language/comments/only_line_comment_and_line.wren
wren_c/test/language/comments/only_line_comment.wren
wren_c/test/language/comments/unicode.wren
wren_c/test/language/empty_file.wren
wren_c/test/language/no_trailing_newline.wren
