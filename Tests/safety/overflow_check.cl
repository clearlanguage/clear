function main() -> int32:
    let x: int8 = 120
    for i in 0..10:
        x += 1
    print(x)
    return 0

// flags: --checks
// expect-exit: -6
