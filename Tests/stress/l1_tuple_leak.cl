// stress test L1_tuple_leak
// expect:
// ab 1
// x 2


function split(s: str) -> (String, int):
    return String(s), 1
function main() -> int32:
    let a, b = split("ab")
    print(a, b)
    let xs = List[(String, int)]()
    xs.push((String("x"), 2))
    print(xs[0][0], xs[0][1])
    return 0
