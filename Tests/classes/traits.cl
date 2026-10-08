trait Shape:
    function area(self: *Shape) -> float64
    function scale(self: *Shape, factor: float64)

trait Named:
    function name(self: *Named) -> str

class Circle(Shape, Named):
    radius: float64

    function area(self: *Circle) -> float64:
        return 3.0 * self.radius * self.radius

    function scale(self: *Circle, factor: float64):
        self.radius *= factor

    function name(self: *Circle) -> str:
        return "circle"

class Square(Shape):
    side: float64

    function area(self: *Square) -> float64:
        return self.side * self.side

    function scale(self: *Square, factor: float64):
        self.side *= factor

// static dispatch: one copy of the function per shape type, no vtable
function doubled_area[T: Shape](shape: *T) -> float64:
    shape.scale(2.0)
    return shape.area()

function main() -> int32:
    let c = Circle(1.0)
    let s = Square(3.0)
    print(doubled_area(&c), c.name())
    print(doubled_area(&s))
    return 0

// expect:
// 12.0 circle
// 36.0
