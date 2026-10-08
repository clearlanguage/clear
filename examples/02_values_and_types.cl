const LIMIT = 4                    // a compile-time constant

function main() -> int32:
    let count = 7                  // int (int32), inferred
    let ratio = 2.5                // float64
    let small: uint8 = 200         // literals adapt to the declared type
    let big: int64 = count         // widening is automatic
    let back = big as int32        // narrowing needs `as`
    let flag = true
    let name: str = "clear"
    let empty: int                 // starts at 0, never garbage
    let grid: [LIMIT; int] = {1, 2}  // arrays: missing elements are 0
    let p = &count                 // a pointer
    *p += 1

    print(count, ratio, small, big, back, flag, name, empty, grid)
    print(10 / 3, 10 % 3, 2 ** 10, 7.0 / 2.0, -count)
    print(name == "clear", len(grid), len(name))
    return 0

// expect:
// 8 2.5 200 7 7 true clear 0 [1, 2, 0, 0]
// 3 1 1024 3.5 -8
// true 4 5
