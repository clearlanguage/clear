declare printf(fmt: *int8, args: ...) -> int32

function twice(x: int64) -> int64:
    return x * 2

function half(x: float64) -> float64:
    return x / 2

function main() -> int32:
    let small: int8 = 100
    let a: int32 = small
    let b: int64 = a
    let f: float64 = a
    let g: float32 = 1.5
    let h: float64 = g
    printf("%d %lld %.1f %.1f\n", a, twice(a), f, h)
    printf("%.2f\n", half(3))
    return 0

// expect:
// 100 200 100.0 1.5
// 1.50
