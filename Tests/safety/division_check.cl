function divide(a: int, b: int) -> int:
    return a / b

function main() -> int32:
    print(divide(10, 2))
    print(divide(1, 0))
    return 0

// flags: --checks
// expect:
// 5
// expect-exit: -6
