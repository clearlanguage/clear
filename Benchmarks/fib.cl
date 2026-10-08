function fib(n: int64) -> int64:
    if n < 2:
        return n
    return fib(n - 1) + fib(n - 2)

function main() -> int32:
    print(fib(38))
    return 0
