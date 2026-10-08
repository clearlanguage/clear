declare printf(fmt: *int8, args: ...) -> int32

function main() -> int32:
    let a = 7
    let b = 3
    printf("%d %d %d %d %d\n", a + b, a - b, a * b, a / b, a % b)
    printf("%d\n", 1 + 2 * 3 - 4 / 2)
    printf("%d\n", (1 + 2) * 3)
    return 0

// expect:
// 10 4 21 2 1
// 5
// 9
