trait Shape:
    function area(self: *Shape) -> float64

class Blob:
    size: int

function measure[T: Shape](s: *T) -> float64:
    return s.area()

function main() -> int32:
    let b = Blob(1)
    measure(&b)
    return 0

// expect-error
