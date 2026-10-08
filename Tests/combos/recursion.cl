function fact(n: int64) -> int64:
    return when n <= 1 use 1 otherwise n * fact(n - 1)

function ackermann(m: int, n: int) -> int:
    if m == 0:
        return n + 1
    if n == 0:
        return ackermann(m - 1, 1)
    return ackermann(m - 1, ackermann(m, n - 1))

function is_even(n: int) -> bool:
    return when n == 0 use true otherwise is_odd(n - 1)

function is_odd(n: int) -> bool:
    return when n == 0 use false otherwise is_even(n - 1)

function hanoi(n: int, from: int, to: int, via: int) -> int:
    if n == 0:
        return 0
    return hanoi(n - 1, from, via, to) + 1 + hanoi(n - 1, via, to, from)

function main() -> int32:
    print(fact(20), ackermann(2, 3), is_even(10), is_odd(7), hanoi(10, 1, 3, 2))
    return 0

// expect:
// 2432902008176640000 9 true true 1023
