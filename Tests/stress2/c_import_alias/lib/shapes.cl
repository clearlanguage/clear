const N = 4
const NAME = "shapes"

enum Shape:
    Circle(r: int)
    Empty

enum Color:
    Red
    Blue = 10

class Box[T]:
    v: T

    function get(self) -> T:
        return self.v

function twice(x: int) -> int:
    return x * 2
