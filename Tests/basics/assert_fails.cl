function main() -> int32:
    print("before")
    let x = 3
    assert x > 10, "x too small"
    print("after")
    return 0

// flags: --checks
// expect:
// before
// expect-exit: -6
