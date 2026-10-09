// stress test L3_when_leak
// expect:
// a x


function main() -> int32:
    let s = String("a")
    let c = true
    let r = when c use s otherwise String("x")
    let q = when not c use s otherwise String("x")
    print(r, q)
    return 0
