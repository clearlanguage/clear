declare printf(fmt: *int8, args: ...) -> int32

function bump(p: *int):
    *p = *p + 1

function main() -> int32:
    let x = 41
    bump(&x)
    printf("%d\n", x)
    return 0

// expect:
// 42
