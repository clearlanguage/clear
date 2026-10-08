declare printf(fmt: *int8, args: ...) -> int32
declare malloc(size: uint64) -> *int8
declare free(p: *int8)

function sum(p: *int, n: int) -> int:
    let total = 0
    let i = 0
    while i < n:
        total += p[i]
        i++
    return total
function main() -> int32:
    let arr: [4; int] = {1, 2, 3, 4}
    let p: *int = &arr[0]
    p[1] = 20
    printf("%d %d\n", sum(p, 4), arr[1])
    return 0

// expect:
// 28 20
