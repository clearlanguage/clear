function twice(x: int) -> int:
    return x * 2

// a parameter that is a function
function apply(f: function(int) -> int, x: int) -> int:
    return f(x)

function main() -> int32:
    print(apply(twice, 4))                     // a named function as a value
    print(apply(lambda x: x + 100, 1))         // a lambda: its type comes from apply

    let square: function(int) -> int = lambda n: n * n
    print(square(9))

    let offset = 10
    let shifted = lambda (x: int): x + offset  // captures a copy of offset
    print(shifted(5))
    offset = 1000
    print(shifted(5))                          // still uses the copy (10)
    return 0

// expect:
// 8
// 101
// 81
// 15
// 15
