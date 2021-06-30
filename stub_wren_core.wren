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

class Sequence {
  all(f) {
    var result = true
    for (element in this) {
      result = f.call(element)
      if (!result) return result
    }
    return result
  }

  any(f) {
    var result = false
    for (element in this) {
      result = f.call(element)
      if (result) return result
    }
    return result
  }

  contains(element) {
    for (item in this) {
      if (element == item) return true
    }
    return false
  }

  count {
    var result = 0
    for (element in this) {
      result = result + 1
    }
    return result
  }

  count(f) {
    var result = 0
    for (element in this) {
      if (f.call(element)) result = result + 1
    }
    return result
  }

  each(f) {
    for (element in this) {
      f.call(element)
    }
  }

  isEmpty { iterate(null) ? false : true }

  // map(transformation) { MapSequence.new(this, transformation) }

  // skip(count) {
  //   if (!(count is Num) || !count.isInteger || count < 0) {
  //     Fiber.abort("Count must be a non-negative integer.")
  //   }

  //   return SkipSequence.new(this, count)
  // }

  // take(count) {
  //   if (!(count is Num) || !count.isInteger || count < 0) {
  //     Fiber.abort("Count must be a non-negative integer.")
  //   }

  //   return TakeSequence.new(this, count)
  // }

  // where(predicate) { WhereSequence.new(this, predicate) }

  reduce(acc, f) {
    for (element in this) {
      acc = f.call(acc, element)
    }
    return acc
  }

  // reduce(f) {
  //   var iter = iterate(null)
  //   if (!iter) Fiber.abort("Can't reduce an empty sequence.")

  //   // Seed with the first element.
  //   var result = iteratorValue(iter)
  //   while (iter = iterate(iter)) {
  //     result = f.call(result, iteratorValue(iter))
  //   }

  //   return result
  // }

  join() { join("") }

  join(sep) {
    var first = true
    var result = ""

    for (element in this) {
      if (!first) result = result + sep
      first = false
      result = result + element.toString
    }

    return result
  }

  // toList {
  //   var result = List.new()
  //   for (element in this) {
  //     result.add(element)
  //   }
  //   return result
  // }
}

class System {
  static print() {
    writeString_("\n")
  }

  static print(obj) {
    writeObject_(obj)
    writeString_("\n")
    return obj
  }

  static printAll(sequence) {
    for (object in sequence) writeObject_(object)
    writeString_("\n")
  }

  static write(obj) {
    writeObject_(obj)
    return obj
  }

  static writeAll(sequence) {
    for (object in sequence) writeObject_(object)
  }

  static writeObject_(obj) {
    var string = obj.toString
    if (string is String) {
      writeString_(string)
    } else {
      writeString_("[invalid toString]")
    }
  }
}