// patterns that move values and are fine: a variable is only unusable while it is empty
// (Token cleans up itself and has no operator copy, so it moves instead of being copied)
class Token:
    text: str

    operator destruct(self):
        self.text = ""

function take(s: Token):
    print("took", s.text)

macro swap(a, b):
    let tmp = a
    a = b
    b = tmp

function pick(flag: bool) -> Token:
    let a = Token("a")
    if flag:
        return a            // this path ends here
    print("still have", a.text)
    return a

function main() -> int32:
    let a = Token("one")
    take(a)
    a = Token("two")       // a new value: usable again
    print(a.text)

    let b = Token("loop")
    for i in 0..5:
        if i == 2:
            take(b)
            break           // moved only once
    
    let c = Token("refill")
    let n = 0
    while n < 2:
        take(c)
        c = Token("again") // refilled before the next time round
        n += 1

    for i in 0..2:
        let inner = Token("inner")
        take(inner)         // a new variable every time round

    let x = Token("x")
    let y = Token("y")
    swap!(x, y)
    print(x.text, y.text)
    print(pick(true).text, pick(false).text)

    let d = Token("d")
    if n > 100:
        take(d)
    else:
        take(d)
    d = Token("d2")
    print(d.text)
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
