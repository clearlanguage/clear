// stress test M2_tuple_neg
// expect:
// 1 -3.0 -1 true


function main() -> int32:
    let t = (1, -3.0)
    let u = (-1, not false)
    print(t[0], t[1], u[0], u[1])
    return 0
