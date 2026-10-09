// stress test H6_generic_is_not
// expect:
// false true
// true false


function check[K](k: K):
    let r: ?int = none
    if k > 0:
        r = 1
    print(r is none, r is not none)
function main() -> int32:
    check(5)
    check(-5)
    return 0
