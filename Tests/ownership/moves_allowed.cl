// patterns that move values and are fine: a variable is only unusable while it is empty
import "string"

function take(s: String):
    print("took", s)

macro swap(a, b):
    let tmp = a
    a = b
    b = tmp

function pick(flag: bool) -> String:
    let a = String("a")
    if flag:
        return a            // this path ends here
    print("still have", a)
    return a

function main() -> int32:
    let a = String("one")
    take(a)
    a = String("two")       // a new value: usable again
    print(a)

    let b = String("loop")
    for i in 0..5:
        if i == 2:
            take(b)
            break           // moved only once
    
    let c = String("refill")
    let n = 0
    while n < 2:
        take(c)
        c = String("again") // refilled before the next time round
        n += 1

    for i in 0..2:
        let inner = String("inner")
        take(inner)         // a new variable every time round

    let x = String("x")
    let y = String("y")
    swap!(x, y)
    print(x, y)
    print(pick(true), pick(false))

    let d = String("d")
    if n > 100:
        take(d)
    else:
        take(d)
    d = String("d2")
    print(d)
    return 0

// expect:
// took one
// two
// took loop
// took refill
// took again
// took inner
// took inner
// y x
// still have a
// a a
// took d
// d2
