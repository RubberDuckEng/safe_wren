// Briefly I made importing trash the stack, this tests
// to make sure we maintin the stack (and thus locals)
// across imports.
{
    var a = 1
    import "./module" for Module
    // expect: ran module
    System.print(Module) // expect: from module
    System.print(a) // expect: 1
}