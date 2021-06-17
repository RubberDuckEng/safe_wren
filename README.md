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

## Longer off
* Garbage Collection
* C API
* Closures / Upvalues
* Limit tests
* Fibers

### Passing Tests:
(Some of these are error tests and are "passing" for the wrong reasons.
e.g. we might throw a lexer error even though the test is expecting
a grammar error, but the harness doesn't distinguish and thus passes.)


78 tests passed. 795 tests failed.
wren_c/test/core/number/decimal_point_at_eof.wren
wren_c/test/core/number/multiply.wren
wren_c/test/core/number/plus.wren
wren_c/test/language/assignment/associativity.wren
wren_c/test/language/assignment/global.wren
wren_c/test/language/assignment/is.wren
wren_c/test/language/assignment/prefix_operator.wren
wren_c/test/language/assignment/undefined.wren
wren_c/test/language/class/attributes/invalid_toplevel.wren
wren_c/test/language/class/missing_class_after_foreign.wren
wren_c/test/language/comments/block.wren
wren_c/test/language/comments/block_at_eof.wren
wren_c/test/language/comments/line_at_eof.wren
wren_c/test/language/comments/only_line_comment.wren
wren_c/test/language/comments/only_line_comment_and_line.wren
wren_c/test/language/comments/unicode.wren
wren_c/test/language/comments/unterminated_block.wren
wren_c/test/language/comments/unterminated_nested_block.wren
wren_c/test/language/conditional/conditional_in_then.wren
wren_c/test/language/conditional/missing_colon.wren
wren_c/test/language/conditional/missing_condition.wren
wren_c/test/language/conditional/missing_else.wren
wren_c/test/language/conditional/missing_question.wren
wren_c/test/language/conditional/missing_then.wren
wren_c/test/language/empty_file.wren
wren_c/test/language/field/outside_class.wren
wren_c/test/language/for/newline_before_in.wren
wren_c/test/language/function/newline_in_expression_block.wren
wren_c/test/language/function/no_parameters.wren
wren_c/test/language/if/newline_after_else.wren
wren_c/test/language/interpolation/unterminated.wren
wren_c/test/language/list/duplicate_comma.wren
wren_c/test/language/list/duplicate_trailing_comma.wren
wren_c/test/language/list/empty_list_with_comma.wren
wren_c/test/language/map/bad_key_precedence.wren
wren_c/test/language/map/duplicate_comma.wren
wren_c/test/language/map/duplicate_trailing_comma.wren
wren_c/test/language/map/eof_after_colon.wren
wren_c/test/language/map/eof_after_key.wren
wren_c/test/language/module/missing_for.wren
wren_c/test/language/module/missing_string_after_import.wren
wren_c/test/language/module/module_dir/something/module.wren
wren_c/test/language/module/name_collision.wren
wren_c/test/language/nonlocal/nonlocal_without_initializer.wren
wren_c/test/language/no_trailing_newline.wren
wren_c/test/language/number/scientific_floating_exponent.wren
wren_c/test/language/number/scientific_float_missing_exponent.wren
wren_c/test/language/number/scientific_missing_exponent.wren
wren_c/test/language/number/scientific_multiple_exponants.wren
wren_c/test/language/number/scientific_multiple_exponent_signs.wren
wren_c/test/language/semicolon.wren
wren_c/test/language/shebang/shebang_at_other_line.wren
wren_c/test/language/shebang/shebang_invalid.wren
wren_c/test/language/static_field/outside_class.wren
wren_c/test/language/string/incomplete_byte_escape_at_eof.wren
wren_c/test/language/string/incomplete_unicode_escape_at_eof.wren
wren_c/test/language/string/unterminated.wren
wren_c/test/language/string/unterminated_raw.wren
wren_c/test/language/unexpected_character.wren
wren_c/test/language/variable/global_in_initializer.wren
wren_c/test/language/variable/global_without_initializer.wren
wren_c/test/language/variable/local_in_initializer.wren
wren_c/test/language/variable/newline_after_equals.wren
wren_c/test/language/variable/use_false_as_var.wren
wren_c/test/language/variable/use_true_as_var.wren
wren_c/test/language/whitespace.wren
wren_c/test/limit/many_globals.wren
wren_c/test/limit/too_many_function_parameters.wren
wren_c/test/limit/too_much_interpolation_nesting.wren
wren_c/test/regression/429.wren
test/bringup/add.wren
test/bringup/do_nothing_while.wren
test/bringup/greater_than.wren
test/bringup/locals.wren
test/bringup/number_parsing.wren
test/bringup/simple_while.wren
test/bringup/trailing_newlines.wren
wren_c/example/hello.wren

### Leads to pursue
* Making InputManager an Iterator, could make easier the "skip until" pattern?
* https://docs.rs/once_cell/1.8.0/once_cell/
* https://docs.rs/anyhow/1.0.41/anyhow/