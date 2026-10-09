class Noisy:
    name: str

    operator destruct(self):
        if self.name != null:
            print("destroy", self.name)

class Pair:              // no destruct of its own: cleans up its fields
    a: Noisy
    b: Noisy

function make(name: str) -> Noisy:
    return Noisy(name)

function consume(n: Noisy):
    print("consumed", n.name)

function main() -> int32:
    let a = Noisy("a")
    if true:
        let b = Noisy("b")
        print("inner end")
    let c = a                    // moves: a is now empty
    print("moved", c.name)
    consume(make("temp"))
    make("dropped")              // made and thrown away
    let p = Pair(Noisy("p1"), Noisy("p2"))
    for i in 0..2:
        let loop = Noisy("loop")
        if i == 1:
            break
    c = Noisy("replacement")     // old c is cleaned up first
    print(make("printed").name)
    print("end of main")
    return 0

// expect:
// inner end
// destroy b
// moved a
// consumed temp
// destroy temp
// destroy dropped
// destroy loop
// destroy loop
// destroy a
// printed
// end of main
// destroy printed
// destroy p2
// destroy p1
// destroy replacement
