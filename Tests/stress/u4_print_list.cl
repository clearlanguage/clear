// stress test U4_print_list
// expect:
// [1, 2]
// {a: 1}


function main() -> int32:
    let xs = List[int]()
    xs.push(1)
    xs.push(2)
    print(xs)
    let m = Map[str, int]()
    m["a"] = 1
    print(m)
    return 0
