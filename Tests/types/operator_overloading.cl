class Vec2:
    x: float64
    y: float64

    function __add__(self: *Vec2, other: Vec2) -> Vec2:
        return Vec2 { self.x + other.x, self.y + other.y }

    function __sub__(self: *Vec2, other: Vec2) -> Vec2:
        return Vec2 { self.x - other.x, self.y - other.y }

    function __mul__(self: *Vec2, k: float64) -> Vec2:
        return Vec2 { self.x * k, self.y * k }

    function __eq__(self: *Vec2, other: *Vec2) -> bool:
        return self.x == other.x and self.y == other.y

    function __lt__(self: *Vec2, other: Vec2) -> bool:
        return self.length2() < other.length2()

    function length2(self: *Vec2) -> float64:
        return self.x * self.x + self.y * self.y

function main() -> int32:
    let a = Vec2 { 1.0, 2.0 }
    let b = Vec2 { 3.0, 4.0 }
    let c = a + b * 2
    print(c)
    print((b - a) + Vec2 { 0.5, 0.5 })
    print(a == Vec2 { 1.0, 2.0 }, a != b, a < b, b < a)
    let total = Vec2 { 0.0, 0.0 }
    for i in 0..3:
        total = total + a
    print(total)
    return 0

// expect:
// Vec2(x=7.0, y=10.0)
// Vec2(x=2.5, y=2.5)
// true true true false
// Vec2(x=3.0, y=6.0)
