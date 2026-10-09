// N4: properties and operators overridden in a subclass run the object's own version, like methods
class A:
    x: int

    property shown(self) -> int:
        return self.x

    property shown(self, value: int):
        self.x = value

    function twice(self) -> int:
        return self.shown * 2

    operator str(self) -> str:
        return "A"

    operator equals(self, other: *A) -> bool:
        return self.x == other.x

    operator get(self, i: int) -> int:
        return self.x + i

class B(A):
    property shown(self) -> int:
        return self.x + 100

    property shown(self, value: int):
        self.x = value * 10

    operator str(self) -> str:
        return "B"

    operator equals(self, other: *A) -> bool:
        return true

    operator get(self, i: int) -> int:
        return -i

function show(p: *A):
    print(*p, p.shown, p.twice(), *p == A(99), p[5])

function main() -> int32:
    let b = B(1)
    let p: *A = &b
    print(b.shown, p.shown, p.twice(), b.twice())
    print(b)
    show(&b)
    show(&A(7))

    p.shown = 2
    print(b.x)
    p.shown += 1
    print(b.x)

    // a copy of the A part of a B is an A
    let copy = *p
    print(copy, copy.shown)
    return 0

// expect:
// 101 101 202 202
// B
// B 101 202 true -5
// A 7 14 false 12
// 20
// 1210
// A 1210
