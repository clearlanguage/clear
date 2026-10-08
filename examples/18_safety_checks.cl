// Compile-time: these are errors before the program ever runs:
//     let a: [3; int] = {1, 2, 3}
//     print(a[5])            // constant index out of range
//     print(10 / 0)          // division by a constant zero
//     let n: int = none      // none is only for ?T
//
// Run time (debug builds, or --checks): the program stops with a clear message instead of
// corrupting memory. This one stops at the out-of-range index:
//     panic: index out of range for an array of 3 (18_safety_checks.cl:15:18)

function main() -> int32:
    let values: [3; int] = {1, 2, 3}
    let i = 5
    print("about to index")
    print(values[i])
    print("never printed")
    return 0

// flags: --checks
// expect:
// about to index
// expect-exit: -6
