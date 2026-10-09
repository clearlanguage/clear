// stress test M10_double_pointer
// expect:
// 7 7


function main() -> int32:
    let a = 7
    let p = &a
    let pp: **int = &p
    print(**pp, *(*pp))
    return 0
