var foo = Fn.new { 1 }
System.print(foo.call()) // expect: 1

var bar = Fn.new {
    return 2
}
System.print(bar.call()) // expect: 2

var baz = Fn.new {
    3
}
System.print(baz.call()) // expect: null