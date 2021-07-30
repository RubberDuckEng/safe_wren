class Foo {
  construct new() { System.print("Foo constructor") }
  getter { "Foo.getter" }
  setter=(value) { "Foo.setter = %(value)" }
  method() { "Foo.method()" }
  [a] { "Foo.[%(a)]" }
  [a]=(value) { "Foo.[%(a)] = %(value)" }
}

class Bar is Foo {
  construct new() { super() }
  getter { super.getter }
  setter=(value) { super.setter = value }
  method() { super.method() }
  [a] { super[a] } // expect runtime error: Foo does not implement ''.
  [a]=(value) { super[a] = value }
}

var bar = Bar.new() // expect: Foo constructor
System.print(bar.getter) // expect: Foo.getter
System.print(bar.setter = 1) // expect: Foo.setter = 1
System.print(bar.method()) // expect: Foo.method()
System.print(bar[1])
// FIXME: Should these work?
// System.print(bar[1]) could expect Foo.[1]
// System.print(bar[1] = 2) could expect Foo.[1] = 2