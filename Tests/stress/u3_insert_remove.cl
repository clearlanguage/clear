// stress test U3_insert_remove
// expect:
// 1 9 3
// 9 3


function main() -> int32:
    let xs = List[int]()
    xs.push(1)
    xs.push(3)
    xs.insert(1, 9)
    print(xs[0], xs[1], xs[2])
    xs.remove(0)
    print(xs[0], xs[1])
    return 0
