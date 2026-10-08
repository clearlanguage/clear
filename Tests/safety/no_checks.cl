function main() -> int32:
    let x: int8 = 127
    x += 1
    assert false, "asserts are removed without checks"
    print(x)
    return 0

// flags: --no-checks
// expect:
// -128
