declare printf(fmt: *int8, args: ...) -> int32

function main() -> int32:
    let a = 6
    printf("%d %d %d %d %d\n", a & 3, a | 1, a ^ 5, a << 2, a >> 1)
    let b = 12
    b &= 10
    b |= 1
    b <<= 1
    printf("%d\n", b)
    printf("%d %d\n", -8 >> 1, ~5)
    return 0

// expect:
// 2 7 3 24 3
// 18
// -4 -6
