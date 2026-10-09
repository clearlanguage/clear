// N11: struct literals with named fields; N29: positional and keyword values together for a derived class
class P:
    x: int
    y: int
    label: str = "p"

class Base:
    a: int
    b: int = 7

class D(Base):
    c: int
    name: String

function main() -> int32:
    let p = P { y = 2 }
    print(p.x, p.y, p.label)
    let q = P { 1, label = "q" }
    print(q.x, q.y, q.label)
    let r = P { label = "r", x = 5, y = 6 }
    print(r)

    let d = D(1, c = 5)
    print(d.a, d.b, d.c, len(d.name))
    let e = D(1, 2, name = String("e"))
    print(e.a, e.b, e.c, e.name)
    let f = D { c = 3, name = String("f") }
    print(f.a, f.b, f.c, f.name)
    let base = Base(b = 1, a = 2)
    print(base.a, base.b)
    return 0

// expect:
// 0 2 p
// 1 0 q
// P(x=5, y=6, label=r)
// 1 7 5 0
// 1 2 0 e
// 0 7 3 f
// 2 1
