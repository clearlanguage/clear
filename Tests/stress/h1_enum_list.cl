// stress test H1_enum_list
// expect:
// 50 49


enum E:
    A(x: int64)
    B
function main() -> int32:
    let xs = List[E]()
    for i in 0..50:
        xs.push(E.A(i))
    switch xs[49]:
        case A(x):
            print(len(xs), x)
        case B:
            print("b")
    return 0
