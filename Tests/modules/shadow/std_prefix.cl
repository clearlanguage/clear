// "math" finds math.cl next to this file (with a warning), "std/math" is always the standard one
import "math"
import "std/math" as stdmath

function main() -> int32:
    print(triple(5), stdmath.sqrt(16.0))
    return 0

// expect-warning: hides the standard ‘math’
// expect:
// 15 4.0
