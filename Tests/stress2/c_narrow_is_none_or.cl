// `if x is none or y is none: return` leaves both holding a value after it; x.value still works too
function find(x: int) -> ?int:
    if x > 0:
        return x
    return none

function both(a: int, b: int) -> int:
    let x = find(a)
    let y = find(b)
    if x is none or y is none:
        return -1
    return x + y.value

function first(a: int) -> int:
    let x = find(a)
    if x is not none:
        return x * 2 + x.value
    return 0

function main() -> int32:
    print(both(1, 2), both(0, 2), both(3, 0))
    print(first(5), first(-5))
    return 0

// expect:
// 3 -1 -1
// 15 0
