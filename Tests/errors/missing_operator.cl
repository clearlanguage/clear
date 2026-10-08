class P:
    x: int

function main() -> int32:
    let a = P { 1 }
    let b = a + a
    return 0

// expect-error
