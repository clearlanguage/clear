enum Result[T]:
    Ok(value: T)
    Err

function main() -> int32:
    return 0

// expect-error: generic enums aren't supported
