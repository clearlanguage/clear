function main() -> int32:
    let x = 10
    if x > 10:
        print("big")
    else if x == 10:
        print("ten")
    else:
        print("small")

    let total = 0
    for i in 0..5:                 // 0 1 2 3 4
        total += i
    for i in 1..=3:                // 1 2 3 (inclusive)
        total += i
    print(total)

    let n = 0
    while true:
        n += 1
        if n % 2 == 0:
            continue
        if n > 7:
            break
    print(n)

    let values: [4; int] = {3, 9, 4, 1}
    for v in values:
        print(v, when v > 3 use "large" otherwise "small")

    switch x:
        case 1, 2:
            print("one or two")
        case 10:
            print("ten again")
        default:
            print("something else")

    print(9 in values, 5 not in values, "ell" in "hello")
    assert total == 16, "total should be 16"

    defer print("deferred: printed last, when main exits")
    print("end of main")
    return 0

// expect:
// ten
// 16
// 9
// 3 small
// 9 large
// 4 large
// 1 small
// ten again
// true true true
// end of main
// deferred: printed last, when main exits
