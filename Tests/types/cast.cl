declare printf(fmt: *int8, args: ...) -> int32

function main() -> int32:
    printf("%d\n", 7.9 as int)
    return 0

// expect:
// 7
