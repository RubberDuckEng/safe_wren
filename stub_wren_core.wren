class Bool {}
// class Fn can't be defined in wren_core, because it's needed
// to be the class for all the functions in wren core!
// Comment out to avoid changing test results for now until
// either of these are implemented at all:
// class Fiber {}
class Null {}
class Num {}
class String {}
class Range {}
class System {}
//   static print() {
//     writeString_("\n")
//   }

// //   static print(obj) {
// //     writeObject_(obj)
// //     writeString_("\n")
// //     return obj
// //   }

//   static printAll(sequence) {
//     for (object in sequence) writeObject_(object)
//     writeString_("\n")
//   }

//   static write(obj) {
//     writeObject_(obj)
//     return obj
//   }

//   static writeAll(sequence) {
//     for (object in sequence) writeObject_(object)
//   }

//   static writeObject_(obj) {
//     var string = obj.toString
//     if (string is String) {
//       writeString_(string)
//     } else {
//       writeString_("[invalid toString]")
//     }
//   }
// }