class Foo {
    construct new() {}
    bar {
        foo()
    }
    foo() { "foo" }
}
var foo = Foo.new()
System.print(foo.bar) // expect: foo
System.print(foo.foo()) // expect: foo
