// N2: a temporary made on the side of `and` / `or` / `when` that did not run must not be cleaned up

function make() -> String:
    return String("x")

function count_made(n: int) -> int:
    let total = 0
    for i in 0..n:
        if i % 2 == 0 and make() == "x":
            total += 1
        if i % 3 == 0 or len(make()) == 1 and (i > 2 and make() == "x"):
            total += 10
    return total

async function later(n: int) -> int64:
    await pause()
    return when n > 0 use len(make()) otherwise 0

function main() -> int32:
    let n = 0
    if n > 0 and make() == "x":
        print("yes")
    let b = n > 0 and len(make()) == 1
    print(b)
    if n == 0 or make() == "x":
        print("or")
    let k = when n > 0 use len(make()) otherwise 0
    print(k)
    let k2 = when n == 0 use len(make()) otherwise len(make()) + 1
    print(k2)

    let l = List[String]()
    let x = String("x")
    if not l.is_empty() and l.last() == x:
        print("last")
    l.push(String("x"))
    if not l.is_empty() and l.last() == x:
        print("last is x")

    if n > 5:
        print("big")
    else if make() == "y":
        print("y")
    else if n == 0 and make() == "x":
        print("elif")

    let i = 0
    while i < 3 and make() == "x":
        i += 1
    print(i)

    print(count_made(7))
    print(later(1).run(), later(0).run())
    return 0

// expect:
// false
// or
// 0
// 1
// last is x
// elif
// 3
// 54
// 1 0
