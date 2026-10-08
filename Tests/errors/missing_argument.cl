function f(a: int, b: int, c: int = 0) -> int:
    return a + b + c

function main() -> int32:
    return f(1, c = 2)

// expect-error
