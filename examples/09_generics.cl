function largest[T](a: T, b: T) -> T:
    return when a > b use a otherwise b

class Box[T]:
    value: T

    function get(self) -> T:
        return self.value

class Pair[A, B]:
    first: A
    second: B

function main() -> int32:
    print(largest(3, 9), largest(2.5, 1.5))     // T is inferred
    print(largest[float64](1, 2))               // or given

    let b = Box(42)                             // Box[int]
    let s = Box[str]("text")
    print(b.get(), s.get())

    let p = Pair[int, bool](1, true)
    print(p.first, p.second)
    return 0

// expect:
// 9 2.5
// 2.0
// 42 text
// 1 true
