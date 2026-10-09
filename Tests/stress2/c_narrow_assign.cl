// assigning none (or another optional) to a narrowed optional sets the optional and ends the narrowing (N40)
function maybe(n: int) -> ?int:
    if n > 0:
        return n
    return none

function main() -> int32:
    let r: ?int = 5
    if r:
        r += 1
        print(r)
        r = none
        print(r is none)
    print(r is none)

    let s: ?String = String("x")
    if s:
        s = none
    print(s is none)

    let q: ?int = 1
    if q is not none:
        q = maybe(7)
        print(q ?? 0)
    return 0

// expect:
// 6
// true
// true
// true
// 7
