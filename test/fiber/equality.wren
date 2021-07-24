// Not structurally equal.
System.print(Fiber.new { 123 } == Fiber.new { 123 })  // expect: false
System.print(Fiber.new { 123 } != Fiber.new { 123 })  // expect: true

// Not equal to other types.
System.print(Fiber.new { 123 } == 1)         // expect: false
System.print(Fiber.new { 123 } == false)     // expect: false
System.print(Fiber.new { 123 } == "fiber 123")  // expect: false
System.print(Fiber.new { 123 } != 1)         // expect: true
System.print(Fiber.new { 123 } != false)     // expect: true
System.print(Fiber.new { 123 } != "fiber 123")  // expect: true

// Equal by identity.
var f = Fiber.new { 123 }
System.print(f == f) // expect: true
System.print(f != f) // expect: false
