// an imported const is a compile-time constant here too
import "lib/shapes"

function main() -> int32:
    let a: [N; int] = {}
    print(len(a), N)
    return 0

// expect:
// 4 4
