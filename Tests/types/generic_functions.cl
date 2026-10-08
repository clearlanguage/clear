function max[T](a: T, b: T) -> T:
    return when a > b use a otherwise b

function swap[T](a: *T, b: *T):
    let tmp = *a
    *a = *b
    *b = tmp

function sum[T](values: *T, count: int) -> T:
    let total: T = 0
    for i in 0..count:
        total += values[i]
    return total

function power[T](base: T, exp: int) -> T:
    if exp == 0:
        return 1
    return base * power(base, exp - 1)

function main() -> int32:
    print(max(3, 9), max(2.5, 1.5), max[float64](1, 2))
    let x = 1
    let y = 2
    swap(&x, &y)
    print(x, y)
    let data: [4; float64] = {1.5, 2.5, 3.0, 4.0}
    print(sum(&data[0], 4))
    print(power(2, 10), power(1.5, 2))
    return 0

// expect:
// 9 2.5 2.0
// 2 1
// 11.0
// 1024 2.25
