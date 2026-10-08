declare printf(fmt: *int8, args: ...) -> int32

function classify(x: int) -> int:
    if x > 10:
        return 1
    elseif x == 10:
        return 0
    else:
        return -1

function main() -> int32:
    printf("%d %d %d\n", classify(11), classify(10), classify(9))
    return 0

// expect:
// 1 0 -1
