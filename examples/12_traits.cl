// a trait lists methods a class promises to have
trait Shape:
    function area(self) -> float64
    function name(self) -> str

class Circle(Shape):
    radius: float64

    function area(self) -> float64:
        return 3.0 * self.radius * self.radius

    function name(self) -> str:
        return "circle"

class Square(Shape):
    side: float64

    function area(self) -> float64:
        return self.side * self.side

    function name(self) -> str:
        return "square"

// only types that satisfy Shape are accepted; each call is resolved at compile time
function report[T: Shape](shape: *T):
    print(shape.name(), shape.area())

function main() -> int32:
    let c = Circle(2.0)
    let s = Square(3.0)
    report(&c)
    report(&s)
    return 0

// expect:
// circle 12.0
// square 9.0
