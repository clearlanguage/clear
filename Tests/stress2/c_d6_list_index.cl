// a failed check inside List names the program's line, not the standard library's
function main() -> int32:
    let xs = List[int]()
    xs.push(1)
    let i = len(xs)
    print(xs[0])
    print(xs[i])
    return 0

// flags: --checks
// expect:
// 1
// expect-exit: -6
// expect-stderr: panic: assertion failed (c_d6_list_index.cl:7:11): List index out of range
