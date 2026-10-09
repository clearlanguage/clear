// last() fails inside its own call to the List's [] operator: still the call in this file is named
function main() -> int32:
    let xs = List[String]()
    xs.push(String("x"))
    print(xs.pop())
    print(xs.last())
    return 0

// flags: --checks
// expect:
// x
// expect-exit: -6
// expect-stderr: (c_d6_list_last.cl:6:18): last of an empty List
