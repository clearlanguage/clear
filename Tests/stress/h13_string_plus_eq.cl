// stress test H13_string_plus_eq
// expect:
// a5 x
// a5 x y


function main() -> int32:
    let s = String("a")
    s += from_int(5)
    let t = String(" x")
    s += t
    print(s)
    let d = String("y")
    s += " " + d
    print(s)
    return 0
