function main() -> int32:
    let s = String("abc")
    let n: int64 = 3
    print(s[n])
    return 0

// flags: --checks
// expect-exit: -6
// expect-stderr: (c_d6_string_index.cl:4:11): String index out of range
