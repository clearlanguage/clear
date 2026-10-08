import "list"

function main() -> int32:
    let xs = List[int]()
    xs.push(1)
    print(xs[0])
    print(xs[1])
    return 0

// flags: --checks
// expect:
// 1
// expect-exit: -6
