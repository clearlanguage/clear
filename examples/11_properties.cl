class Temperature:
    celsius: float64

    // read as t.fahrenheit
    property fahrenheit(self) -> float64:
        return self.celsius * 9.0 / 5.0 + 32.0

    // t.fahrenheit = value
    property fahrenheit(self, value: float64):
        self.celsius = (value - 32.0) * 5.0 / 9.0

function main() -> int32:
    let t = Temperature(100.0)
    print(t.fahrenheit)
    t.fahrenheit = 32.0
    print(t.celsius)
    t.fahrenheit += 18.0
    print(t.celsius)
    return 0

// expect:
// 212.0
// 0.0
// 10.0
