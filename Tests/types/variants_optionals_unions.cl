import "math"

enum Shape:
    Circle(radius: float64)
    Rect(width: float64, height: float64)
    Empty

    function area(self: *Shape) -> float64:
        switch *self:
            case Circle(r):
                return PI * r * r
            case Rect(w, h):
                return w * h
            case Empty:
                return 0.0

    function name(self: *Shape) -> str:
        switch *self:
            case Shape.Circle(_):
                return "circle"
            case Shape.Rect, Shape.Empty:
                return "other"

enum Direction:
    North
    South

function find(values: [4; int], target: int) -> ?int:
    for i in 0..4:
        if values[i] == target:
            return i
    return none

class Point:
    x: int = 1
    y: int = 2

union Bits:
    i: int64
    f: float64

function main() -> int32:
    let shapes: [3; Shape] = {Shape.Circle(1.0), Shape.Rect(height = 3.0, width = 2.0), Shape.Empty}
    for s in shapes:
        print(s, round(s.area() * 100.0) / 100.0, s.name())

    let c = Shape.Circle(2.5)
    print(c is Shape.Circle, c is Shape.Rect, c is not Shape.Empty)

    let data: [4; int] = {5, 7, 9, 11}
    let found = find(data, 9)
    let missing = find(data, 4)
    print(found, missing, found.value, missing.value_or(-1), found is none, missing is none, found is not none)

    switch found:
        case some(index):
            print("found at", index)
        case none:
            print("not found")

    let maybe: ?float64 = 2
    let nothing: ?float64
    print(maybe, nothing, maybe.value_or(0.0) + nothing.value_or(0.5))

    let p = Point(y = 10)
    print(p)

    let b = Bits(f = 1.0)
    print(b.i == 4607182418800017408, b.f)
    b.i = 0
    print(b.f)
    return 0

// expect:
// Shape.Circle(radius=1.0) 3.14 circle
// Shape.Rect(width=2.0, height=3.0) 6.0 other
// Shape.Empty 0.0 other
// true false true
// 2 none 2 -1 false true true
// found at 2
// 2.0 none 2.5
// Point(x=1, y=10)
// true 1.0
// 0.0
