import "math"

enum Color:                        // a plain enum
    Red
    Green
    Blue = 10

enum Shape:                        // cases can carry data
    Circle(radius: float64)
    Rect(width: float64, height: float64)
    Empty

    function area(self: *Shape) -> float64:
        switch *self:              // every case must be handled
            case Circle(r):
                return PI * r * r
            case Rect(w, h):
                return w * h
            case Empty:
                return 0.0

function main() -> int32:
    let c = Color.Blue
    print(c, c as int32)

    switch c:
        case Color.Red:
            print("red")
        case Color.Green, Color.Blue:
            print("green or blue")

    let shapes: [3; Shape] = {Shape.Circle(1.0), Shape.Rect(2.0, 3.0), Shape.Empty}
    for s in shapes:
        print(s, round(s.area() * 100.0) / 100.0)

    let r = Shape.Rect(width = 4.0, height = 1.0)
    print(r is Shape.Rect, r is not Shape.Circle)
    return 0

// expect:
// Color.Blue 10
// green or blue
// Shape.Circle(radius=1.0) 3.14
// Shape.Rect(width=2.0, height=3.0) 6.0
// Shape.Empty 0.0
// true true
