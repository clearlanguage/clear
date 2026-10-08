trait Shape:
    function area(self: *Shape) -> float64

class Blob(Shape):
    size: int

function main() -> int32:
    return 0

// expect-error
