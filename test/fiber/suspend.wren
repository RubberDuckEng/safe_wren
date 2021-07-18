System.print(1) // expect: 1
Fiber.suspend()
System.print(2) // Never printed