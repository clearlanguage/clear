// a number literal doesn't decide a generic type when another argument does: min(5, len(xs)) is min[int64]
import "math"

function main() -> int32:
    let xs = List[int]()
    xs.push(1)
    xs.push(2)
    xs.push(3)
    let a = min(5, len(xs))
    let b = max(-1, len(xs))
    let c = min(len(xs), 5)
    print(a, b, c)
    print(min(2, 7), max(2.5, 1))
    return 0

// expect:
// 3 3 3
// 2 2.5
