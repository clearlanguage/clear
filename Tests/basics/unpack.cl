declare printf(fmt: *int8, args: ...) -> int32

function add3(a: int, b: int, c: int) -> int:
    return a + b + c

function main() -> int32:
    let values: [3; int] = {1, 2, 3}
    let triple = (10, 20, 30)
    print(add3(values...), add3(triple...))
    let pair = (4, 5)
    print(add3(pair..., 6))
    printf("%d-%d-%d\n", values...)
    return 0

// expect:
// 6 60
// 15
// 1-2-3
