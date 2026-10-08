declare printf(fmt: *int8, args: ...) -> int32

class P:
    x: int

function main() -> int32:
    let p = P { 1 }
    let y = p[0]
    return 0

// expect-error
