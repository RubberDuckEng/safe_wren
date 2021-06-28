class Foo {
    construct new() {}
    foo() { "foo" }
    bar { "bar" }
    baz(ignored) { "baz" }

}
var foo = Foo.new()
System.print(foo.foo()) // expect: foo
System.print(foo.bar) // expect: bar
System.print(foo.baz(1)) // expect: baz