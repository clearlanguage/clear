declare printf(fmt: *int8, args: ...) -> int32

class Pair:
    a: int32
    b: int32

function main() -> int32:
    printf("%d %d\n", sizeof int64 as int32, sizeof Pair as int32)
    return 0

// expect:
// 8 8
