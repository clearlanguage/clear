// N12: let x = *xs[0] with an int element says the value is not a pointer (not "needs a type or value")
function main() -> int32:
    let xs = List[int]()
    xs.push(1)
    let x = *xs[0]
    print(x)
    return 0

// expect-error: is a ‘int32’, not a pointer
