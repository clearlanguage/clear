function main() -> int32:
    let nothing: ?int = none
    print("before")
    print(nothing.value)
    return 0

// flags: --checks
// expect:
// before
// expect-exit: -6
