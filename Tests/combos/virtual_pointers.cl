class Shape:
    name: str

    virtual function area(self: *Shape) -> float64:
        return 0.0

class Square(Shape):
    side: float64

    function area(self: *Square) -> float64:
        return self.side * self.side

class Tri(Shape):
    b: float64
    h: float64

    function area(self: *Tri) -> float64:
        return self.b * self.h / 2.0

function describe(s: *Shape):
    print(s.name, s.area())

function main() -> int32:
    let sq = Square("square", 3.0)
    let tr = Tri("tri", 4.0, 5.0)
    let shapes: [2; *Shape] = {&sq, &tr}
    let total = 0.0
    for s in shapes:
        describe(s)
        total += s.area()
    print(total)
    return 0

// expect:
// square 9.0
// tri 10.0
// 19.0
