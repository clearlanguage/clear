function f(a: int, b: int = 1) -> int:
    return a + b

function main() -> int32:
    return f(1, c = 2)

// expect-error
