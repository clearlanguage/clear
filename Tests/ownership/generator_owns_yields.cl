// a generator owns the values it yields: each one is cleaned up when the next replaces it, value() hands out a copy;
// operator copy runs even on a class with nothing to clean up
import "string"
import "map"
class Counted:
    n: int
    operator copy(self) -> Counted:
        print("copying", self.n)
        return Counted(self.n + 100)
function names() -> Generator[String]:
    let mine = String("local")
    yield mine
    for i in 0..3:
        yield String("gen")
    print("still mine:", mine)
function main() -> int32:
    for n in names():
        print(n)
    for n in names():
        print("break at", n)
        break
    let g = names()
    g.advance()
    let first = g.value()
    first.append("!")
    g.advance()
    print(first, g.value())
    let m = Map[String, int]()
    m[String("a")] = 1
    m[String("b")] = 2
    for k in m:
        print("key", k, m[k])
    print(len(m))
    let c = Counted(1)
    let d = c                // d is changed below, so it needs its own copy: operator copy runs
    d.n += 1
    print(c.n, d.n)
    return 0

// expect:
// local
// gen
// gen
// gen
// still mine: local
// break at local
// local! gen
// key a 1
// key b 2
// 2
// copying 1
// 1 102
