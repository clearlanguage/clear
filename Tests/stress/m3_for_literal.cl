// stress test M3_for_literal
// expect:
// 6
// ab


function main() -> int32:
    let total = 0
    for x in {1, 2, 3}:
        total += x
    print(total)
    let s = String("")
    for w in {"a", "b"}:
        s += w
    print(s)
    return 0
