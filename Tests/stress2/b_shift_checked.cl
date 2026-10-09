// N20: with checks a shift by the bit width or more stops the program
// flags: --checks
function main() -> int32:
    let x: uint32 = 0xDEADBEEF
    let s: uint32 = 31
    print(x >> s)
    s += 1
    print(x >> s)
    return 0

// expect:
// 1
// expect-exit: -6
