function twice(x: int) -> int:
    return x * 2

function main() -> int32:
    let f = twice
    print(f(1, 2))
    return 0

// expect-error
