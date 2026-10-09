class Vec2:
    x: float64
    y: float64

    operator add(self, other: Vec2) -> Vec2:
        return Vec2(self.x + other.x, self.y + other.y)

    operator multiply(self, k: float64) -> Vec2:
        return Vec2(self.x * k, self.y * k)

    operator equals(self, other: Vec2) -> bool:
        return self.x == other.x and self.y == other.y

    operator str(self) -> str:     // how print shows it
        return "<vector>"

class Squares:
    operator get(self, i: int64) -> int64:
        return i * i

    operator len(self) -> int64:
        return 4

    operator call(self, x: int) -> int:
        return x * x * x

function main() -> int32:
    let a = Vec2(1.0, 2.0)
    let b = a + Vec2(3.0, 4.0) * 2.0
    print(b.x, b.y, a == Vec2(1.0, 2.0), a != b, a)

    let s = Squares()
    print(s[5], len(s), s(2))
    for v in s:                               // len + get make it iterable
        print(v)
    return 0

// expect:
// 7.0 10.0 true true <vector>
// 25 4 8
// 0
// 1
// 4
// 9
