// no sign warning when it cannot matter: a constant that is >= 0, a wider signed side, the same signedness
function main() -> int32:
    let u: uint32 = 5
    let w: int64 = -1
    let v: uint32 = 7
    const LIMIT = 3
    print(u > 0, u == 5, u > LIMIT, u > w, u < v, u / 2)
    return 0

// expect-no-warning: A signed and an unsigned integer
// expect:
// true true true true true 2
