declare printf(fmt: *int8, args: ...) -> int32

function main() -> int32:
    let arr: [3; int] = {1, 2, 3}
    arr[0] = 10
    let p: *int = &arr[1]
    printf("%d %d %d\n", arr[0], arr[2], *p)
    return 0

// expect:
// 10 3 2
