// L3: `when c use s otherwise String("x")`: one side new, the other not; neither may leak

function pick(n: int) -> String:
    let parts = List[String]()
    parts.push(String("a"))
    let arg = when n > 1 use parts[0] otherwise String("empty")
    return arg

function main() -> int32:
    let s = String("a")
    let c = true
    let r = when c use s otherwise String("x")
    let q = when not c use s otherwise String("x")
    print(r, q)
    print(pick(1), pick(2))
    let total: int64 = 0
    for i in 0..4:
        let t = String("t")
        let v = when i % 2 == 0 use t otherwise String("long")
        total += len(v)
        print(len(when i > 1 use v otherwise String("xy")))
    print(total)
    return 0

// expect:
// a x
// empty a
// 2
// 2
// 1
// 4
// 10
