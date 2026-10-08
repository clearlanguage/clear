function half(x: int32) -> int32:
    return x / 2

function main() -> int32:
    let big: int64 = 10
    return half(big)

// expect-error
