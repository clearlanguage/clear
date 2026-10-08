declare printf(fmt: *int8, args: ...) -> int32

function add(a: int, b: int) -> int:
    return a + b

function fib(n: int) -> int:
    if n < 2:
        return n
    return fib(n - 1) + fib(n - 2)

function main() -> int32:
    printf("%d %d\n", add(2, 3), fib(20))
    return 0

// expect:
// 5 6765
