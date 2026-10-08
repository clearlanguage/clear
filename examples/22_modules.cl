import "math"                       // the standard library
import "lib/geometry"               // a file next to this one (.cl is implied)
import "lib/geometry" as geo        // or reach it through a name

function main() -> int32:
    let s = square(3)
    print(s.area(), geo.square(2).area(), shapes_made)
    print(sqrt(16.0), max(3, 8), clamp(15, 0, 10), gcd(12, 18))
    return 0

// expect:
// 9 4 2
// 4.0 8 10 6
