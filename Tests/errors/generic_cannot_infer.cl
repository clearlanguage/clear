function make[T]() -> T:
    let value: T = 0
    return value

function main() -> int32:
    let x = make()
    return 0

// expect-error
