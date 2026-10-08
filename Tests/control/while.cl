declare printf(fmt: *int8, args: ...) -> int32

function main() -> int32:
    let i = 0
    let total = 0
    while i < 10:
        total += i
        i++
    printf("%d\n", total)
    return 0

// expect:
// 45
