// stress test H2_recursive_enum
// expect:
// 2


enum J:
    A
    B(items: List[J])
function main() -> int32:
    let xs = List[J]()
    xs.push(J.A)
    xs.push(J.A)
    let j = J.B(xs)
    switch j:
        case B(items):
            print(len(items))
        case A:
            print("a")
    return 0
