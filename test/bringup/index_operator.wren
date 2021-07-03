class Foo {
  construct new() { }
  [index] { index }
}
var foo = Foo.new()
System.print(foo[0]) // expect: 0