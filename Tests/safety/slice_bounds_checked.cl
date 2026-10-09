// flags: --checks
function main() -> int32:
    let xs: [3; int] = {1, 2, 3}
    let s = xs[2:1]
    print(len(s))
    return 0

// expect-exit: -6
