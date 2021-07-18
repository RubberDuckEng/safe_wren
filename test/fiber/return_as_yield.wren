var fiber1 = Fiber.new() {
  Fiber.yield(1)
  Fiber.yield(2)
}
System.print(fiber1.call()) // expect: 1
System.print(fiber1.call()) // expect: 2

var fiber2 = Fiber.new() {
  return 1
  return 2
}
System.print(fiber2.call()) // expect: 1
System.print(fiber2.call()) // expect runtime error: Cannot call a finished fiber.