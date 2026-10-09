// stress test H7_deref_copy
// expect:
// 1 2


function main() -> int32:
    let b = List[int]()
    b.push(1)
    let p = &b
    if true:
        let a = *p
        a.push(5)
        print(len(b), len(a))
    return 0
