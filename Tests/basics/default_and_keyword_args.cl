function greet(name: str, greeting: str = "hello", times: int = 1):
    for i in 0..times:
        print(greeting, name)

class Shape:
    w: int

    function scaled(self: *Shape, factor: int = 2) -> int:
        return self.w * factor

function main() -> int32:
    greet("ada")
    greet("bob", "hi")
    greet("cy", times = 2)
    greet(times = 1, name = "dee", greeting = "hey")
    let s = Shape(5)
    print(s.scaled(), s.scaled(3), s.scaled(factor = 10))
    return 0

// expect:
// hello ada
// hi bob
// hello cy
// hello cy
// hey dee
// 10 15 50
