declare printf(fmt: *int8, args: ...) -> int32

function main() -> int32:
    let i = 0
    let odd = 0
    while true:
        i++
        if i > 9:
            break
        if i % 2 == 0:
            continue
        odd += i
    printf("%d %d\n", i, odd)
    return 0

// expect:
// 10 25
