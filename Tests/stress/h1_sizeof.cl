// stress test H1_sizeof
// expect:
// 16
// 20 19 7


class P:
    a: int
    b: int64
function main() -> int32:
    print(sizeof P)
    let xs = List[P]()
    for i in 0..20:
        xs.push(P(i, 7))
    print(len(xs), xs[19].a, xs[19].b)
    return 0
