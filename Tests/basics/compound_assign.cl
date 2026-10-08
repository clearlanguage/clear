declare printf(fmt: *int8, args: ...) -> int32

function main() -> int32:
    let x = 10
    x += 5
    x -= 3
    x *= 2
    x /= 4
    x %= 4
    printf("%d\n", x)
    return 0

// expect:
// 2
