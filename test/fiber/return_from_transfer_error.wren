var A = Fiber.new {
  System.print("transferred to A")
  var foo = B.transferError("error!")
  System.print(foo)
}

var B = Fiber.new {
  System.print("started B")
  A.transfer()
  System.print("should not get here")
}

B.try()
// expect: started B
// expect: transferred to A
System.print(B.error) // expect: error!
System.print(A.error) // expect: null
System.print(A.isDone) // expect: false
A.call("foo")
// expect: "foo"
System.print(A.isDone) // expect: true