function divmod(a: int, b: int) -> (int, int):
    return a / b, a % b

function minmax(values: [4; float64]) -> (float64, float64):
    let low = values[0]
    let high = values[0]
    for v in values:
        if v < low:
            low = v
        if v > high:
            high = v
    return (low, high)

function main() -> int32:
    let t = (1, 2.5, "three")
    print(t, t[0], t[2])
    let q, r = divmod(17, 5)
    print(q, r)
    let (low, high) = minmax({3.0, -1.0, 8.5, 2.0})
    print(low, high)
    let a = 1
    let b = 2
    a, b = b, a
    print(a, b)
    let pair: (int64, float64) = (7, 3)
    print(pair)
    let arr: [3; int] = {4, 5, 6}
    let x, y, z = arr
    print(x + y + z)
    t[0] = 10
    print(t[0])
    return 0

// expect:
// (1, 2.5, three) 1 three
// 3 2
// -1.0 8.5
// 2 1
// (7, 3.0)
// 15
// 10
