// functions can be used before they are defined
function main() -> int32:
    print(add(2, 3))
    print(greet("ada"), greet("ada", greeting = "hi"))
    print(describe(height = 2, width = 5))
    print(factorial(10))
    return 0

function add(a: int, b: int) -> int:
    return a + b

function greet(name: str, greeting: str = "hello") -> str:   // a default value
    return greeting

function describe(width: int, height: int) -> int:
    return width * 100 + height

function factorial(n: int64) -> int64:
    if n <= 1:
        return 1
    return n * factorial(n - 1)

// expect:
// 5
// hello hi
// 502
// 3628800
