// N4: destruction runs on the object's real type: derived operator destruct, then the base's, then the fields
import "memory"

class Base:
    id: int

    operator destruct(self):
        print("base destruct", self.id)

class Derived(Base):
    tag: String

    operator destruct(self):
        print("derived destruct", self.tag)

class Quiet(Base):            // no destruct of its own: the base's runs once
    name: String

class Plain:
    n: int

class Owner(Plain):           // the base owns nothing, the derived class does
    items: List[String]

function make(id: int) -> *Base:
    let p = allocate[Derived](1)
    place(p, Derived(id, String("heap")))
    return p

function main() -> int32:
    // a Derived going out of scope
    if true:
        let d = Derived(1, String("local"))
    print("--")

    // destroy through a *Base pointing at a Derived
    let b = make(2)
    destroy(b)
    release(b)
    print("--")

    // pointers to objects of different types, cleaned up by their owner
    let all = List[*Base]()
    all.push(make(3))
    let q = allocate[Quiet](1)
    place(q, Quiet(4, String("quiet")))
    all.push(q)
    for item in all:
        destroy(item)
        release(item)
    print("--")

    // a base that owns nothing: the derived fields are still freed
    let o = allocate[Owner](1)
    place(o, Owner(5, List[String]()))
    o.items.push(String("x"))
    let asPlain: *Plain = o
    destroy(asPlain)
    release(asPlain)
    print("done")
    return 0

// expect:
// derived destruct local
// base destruct 1
// --
// derived destruct heap
// base destruct 2
// --
// derived destruct heap
// base destruct 3
// base destruct 4
// --
// done
