declare printf(fmt: *int8, args: ...) -> int32
declare malloc(size: uint64) -> *int8
declare free(p: *int8)

function main() -> int32:
    let arr: [4; int] = {1, 2, 3, 4}
    let p: *int = &arr[0]
    let q = p + 2
    printf("%d\n", *q)
    return 0

// expect:
// 3
