declare printf(fmt: *int8, args: ...) -> int32

class Box[T]:
    value: T

function main() -> int32:
    let a = Box { 7 }
    let b = Box[int64] { 9 }
    printf("%d %lld\n", a.value, b.value)
    return 0

// expect:
// 7 9
