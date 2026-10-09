class Point:
    x: int
    y: float64

function main() -> int32:
    let n = 42
    let u: uint8 = 200
    print("hello", n, -7, u)
    print(2.5, 5.0, 0.1, 1.0 / 3.0, 1e20)
    print(true, false, 3 > 2)
    let arr: [3; int] = {1, 2, 3}
    print(arr, Point { 1, 2.5 })
    print("100%")
    print()
    print("done")
    return 0

// expect:
// hello 42 -7 200
// 2.5 5.0 0.1 0.3333333333333333 1e+20
// true false true
// [1, 2, 3] Point(x=1, y=2.5)
// 100%
//
// done
