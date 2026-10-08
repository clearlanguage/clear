declare printf(fmt: *int8, args: ...) -> int32

function main() -> int32:
    let a = 5
    let b = -a
    let c = not (a > 3)
    printf("%d %d\n", b, c)
    return 0

// expect:
// -5 0
