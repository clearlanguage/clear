// methods with their own type parameters, on plain and generic classes
import "list"

class Box:
    n: int

    function apply[F](self, f: F) -> int:
        return f(self.n)

    function pair[U](self, other: U) -> U:
        return other

class Stack[T]:
    items: List[T]

    function push(self, value: T):
        self.items.push(value)

    // U comes from what f returns
    function convert[U](self, f: function(T) -> U) -> Stack[U]:
        let result = Stack[U](List[U]())
        for item in self.items:
            result.push(f(item))
        return result

function main() -> int32:
    let b = Box(5)
    print(b.apply(lambda x: x * 3), b.pair(2.5), b.pair("text"))

    let s = Stack[int](List[int]())
    s.push(1)
    s.push(2)
    let halves = s.convert(lambda x: x as float64 / 2.0)
    let offset = 100
    let shifted = s.convert(lambda x: x + offset)
    print(halves.items[1], shifted.items[0])
    return 0

// expect:
// 15 2.5 text
// 1.0 101
