// Not structurally equal.
System.print({ 1: 2 } == { 1 : 2 })  // expect: false
System.print({ 1: 2 } != { 1 : 2 })  // expect: true

// Not equal to other types.
System.print({ 1: 2 } == 1)         // expect: false
System.print({ 1: 2 } == false)     // expect: false
System.print({ 1: 2 } == "fn 123")  // expect: false
System.print({ 1: 2 } != 1)         // expect: true
System.print({ 1: 2 } != false)     // expect: true
System.print({ 1: 2 } != "fn 123")  // expect: true

// Equal by identity.
var f = { 1: 2 }
System.print(f == f) // expect: true
System.print(f != f) // expect: false
