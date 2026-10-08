class Plain:              // not in any hierarchy: no hidden table, same size as its fields
    a: int64
    b: int64

class Base:               // no methods of its own
    id: int

class Child(Base):
    function name(self) -> str:
        return "child"

class Grandchild(Child):
    function name(self) -> str:
        return "grandchild"

function show(c: *Child):
    print(c.id, c.name())

function main() -> int32:
    print(sizeof Plain)
    let c = Child(1)
    let g = Grandchild(2)
    show(&c)
    show(&g)              // runs Grandchild.name, no keyword needed
    let base: *Base = &g
    print(base.id)
    return 0

// expect:
// 16
// 1 child
// 2 grandchild
// 2
