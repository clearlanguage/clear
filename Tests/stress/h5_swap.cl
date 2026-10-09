// stress test H5_swap
// expect:
// apple pear
// b a


function main() -> int32:
    let a = String("pear")
    let b = String("apple")
    a, b = b, a
    print(a, b)
    let xs = List[String]()
    xs.push(String("a"))
    xs.push(String("b"))
    xs[0], xs[1] = xs[1], xs[0]
    print(xs[0], xs[1])
    return 0
