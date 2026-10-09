// lambdas without parameter types take them from how they are called
import "string"
import "list"

function apply[F](f: F, x: int) -> int:
    return f(x)

function map_list[F](xs: *List[int], f: F) -> List[int]:
    let out = List[int]()
    for x in xs:
        out.push(f(x))
    return out

function main() -> int32:
    print(apply(lambda x: x * 2, 21))
    let offset = 10
    print(apply(lambda x: x + offset, 5))

    let add = lambda a, b: a + b          // one lambda, used with two sets of types
    print(add(1, 2), add(1.5, 2.25))

    let name = String("ada")
    let greet = lambda greeting: print(greeting, name)
    greet("hi")
    greet(42)

    let nums = List[int]()
    for i in 1..=4:
        nums.push(i)
    let squares = map_list(&nums, lambda n: n * n)
    print(squares[0], squares[1], squares[2], squares[3])
    return 0

// expect:
// 42
// 15
// 3 3.75
// hi ada
// 42 ada
// 1 4 9 16
