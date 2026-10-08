declare printf(fmt: *int8, args: ...) -> int32

function main() -> int32:
    let big: int64 = 5
    let small: int32 = big
    return 0

// expect-error
