// self + self in a method: self is a pointer, the operator of the value it points at is used (D3)
class V:
    x: int

    operator add(self, o: V) -> V:
        return V(self.x + o.x)

    operator multiply(self, k: int) -> V:
        return V(self.x * k)

    function twice(self) -> V:
        return self + self

    function triple(self) -> V:
        return self * 3

function main() -> int32:
    let v = V(2)
    let p = &v
    print(v.twice().x, v.triple().x, (p + p).x)
    return 0

// expect:
// 4 6 4
