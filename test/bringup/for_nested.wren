var y = 0
for (x in 0..1) {
    for (z in 0..1) {
        y = y + 1
        System.print(y)
    }
}

// expect: 1
// expect: 2