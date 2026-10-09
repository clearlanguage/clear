// stress test M13_ptr_is_case
// expect:
// true


enum K:
    A(x: int)
    B
function main() -> int32:
    let k = K.A(1)
    let p = &k
    print(p is K.A)
    return 0
