declare printf(fmt: *int8, args: ...) -> int32

function main() -> int32:
    let a = 2
    let b = 3
    // comparisons bind tighter than and/or, and binds tighter than or
    let r1 = a < b and b < 4 or a == 9
    // bitwise binds tighter than comparison
    let r2 = a & 1 == 0
    let r3 = 1 + 2 << 1
    printf("%d %d %d %d\n", r1, r2, r3, -a * b)
    return 0

// expect:
// 1 1 6 -6
