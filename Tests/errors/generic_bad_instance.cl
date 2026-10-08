function double[T](x: T) -> T:
    return x * 2

function main() -> int32:
    let s = double("text")
    return 0

// expect-error
