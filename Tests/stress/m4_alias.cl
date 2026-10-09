// stress test M4_alias
// expect:
// 3 hello 42 16 7


import "m4_pt" as m
function main() -> int32:
    let p = m.Pt(1, 2)
    print(p.x + p.y, m.NAME, m.N, m.sq!(4), m.Box[int](7).v)
    return 0
