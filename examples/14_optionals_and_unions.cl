function find(values: [4; int], target: int) -> ?int:   // an int, or none
    for i in 0..4:
        if values[i] == target:
            return i
    return none

union Bits:                       // all fields share the same 8 bytes
    i: int64
    f: float64

function main() -> int32:
    let data: [4; int] = {5, 7, 9, 11}
    let found = find(data, 9)
    let missing = find(data, 4)

    print(found, missing)
    print(found.value, missing.value_or(-1))
    print(found is none, missing is none, found is not none)

    switch found:
        case some(index):
            print("found at", index)
        case none:
            print("not found")

    let maybe: ?float64 = 2.5      // a plain value converts to an optional
    let nothing: ?float64          // starts as none
    print(maybe.value_or(0.0) + nothing.value_or(0.5))

    let b = Bits(f = 1.0)
    print(b.i)                     // the bit pattern of 1.0
    return 0

// expect:
// 2 none
// 2 -1
// false true true
// found at 2
// 3.0
// 4607182418800017408
