// an optional is its value in the rest of its own and/or condition (N28)
function check(a: ?int) -> int:
    if not a or a < 0:
        return 1
    return a * 10

function main() -> int32:
    let a: ?int = 3
    if a and a > 2:
        print(a)

    let b: ?int = none
    print(b is not none and b > 1, b is none or b < 1, a is not none and a == 3)

    let s: ?String = String("hey")
    print(s and len(s) == 3)

    let n = 0
    while a and a > n:
        n += 1

    print(n, check(a), check(none), check(-4))
    return 0

// expect:
// 3
// false true true
// true
// 3 30 1 1
