// N11: a struct literal naming a field the class does not have is an error
class P:
    x: int
    y: int

function main() -> int32:
    let p = P { z = 2 }
    print(p.x)
    return 0

// expect-error: ‘z’ is not a field of ‘P’
