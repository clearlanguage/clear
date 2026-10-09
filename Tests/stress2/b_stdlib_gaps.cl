// N30 String comparisons, N31 x in list, N34 String(), N37 from_float/append_float like print
function main() -> int32:
    let s = String("m")
    let t = String("n")
    print(t > s, s <= t, t >= s, s < t, s > t, s >= String("m"), t <= s)
    print(s < "n", t > "a", "a" < s)

    let xs = List[int]()
    xs.push(3)
    xs.push(9)
    print(3 in xs, 4 in xs, 4 not in xs, 9 not in xs)
    let names = List[String]()
    names.push(String("ann"))
    let who = String("bob")
    print("ann" in names, who in names, who)

    let empty = String()
    empty.append("x")
    print(len(String()), empty)

    let values = [123456789.0, 2.0, 0.1, 0.1 + 0.2, 1e20, -0.5, 1.5e-7, 100.0]
    for v in values:
        let text = from_float(v)
        let more = String("=")
        more.append_float(v)
        print(v, text, more)
    return 0

// expect:
// true true true true false true false
// true true true
// true false true false
// true false bob
// 0 x
// 123456789.0 123456789.0 =123456789.0
// 2.0 2.0 =2.0
// 0.1 0.1 =0.1
// 0.30000000000000004 0.30000000000000004 =0.30000000000000004
// 1e+20 1e+20 =1e+20
// -0.5 -0.5 =-0.5
// 1.5e-07 1.5e-07 =1.5e-07
// 100.0 100.0 =100.0
