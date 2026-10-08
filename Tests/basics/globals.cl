declare printf(fmt: *int8, args: ...) -> int32

let counter = 5

function main() -> int32:
    printf("%d\n", counter)
    return 0

// expect:
// 5
