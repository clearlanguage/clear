class Shape:
    name: str

    virtual function area(self: *Shape) -> float64:
        return 0.0

    function describe(self: *Shape):
        print(self.name, self.area())

class Rect(Shape):
    w: float64
    h: float64

    function area(self: *Rect) -> float64:
        return self.w * self.h

    function describe(self: *Rect):
        print("rectangle:")
        super.describe()

class Square(Rect):
    property side(self: *Square) -> float64:
        return self.w

    property side(self: *Square, value: float64):
        self.w = value
        self.h = value

    property perimeter(self: *Square) -> float64:
        return 4.0 * self.w

class Temperature:
    celsius: float64

    property fahrenheit(self: *Temperature) -> float64:
        return self.celsius * 9.0 / 5.0 + 32.0

    property fahrenheit(self: *Temperature, value: float64):
        self.celsius = (value - 32.0) * 5.0 / 9.0

function main() -> int32:
    let r = Rect("r", 2.0, 3.0)
    r.describe()

    let s = Square("sq", 1.0, 1.0)
    s.side = 5.0
    print(s.side, s.area(), s.perimeter)
    s.side += 1.0
    print(s.side, s.area())

    let shape: *Shape = &s
    shape.describe()

    let t = Temperature(100.0)
    print(t.fahrenheit)
    t.fahrenheit = 32.0
    print(t.celsius)

    let p = &t
    p.fahrenheit = 212.0
    print(p.celsius, p.fahrenheit)
    return 0

// expect:
// rectangle:
// r 6.0
// 5.0 25.0 20.0
// 6.0 36.0
// sq 36.0
// 212.0
// 0.0
// 100.0 212.0
