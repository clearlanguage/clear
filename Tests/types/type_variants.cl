variant Number:
    int
    float64

variant Value:
    int64
    str
    bool

function describe(v: Value) -> str:
    switch v:
        case int64(n):
            return when n > 10 use "big number" otherwise "small number"
        case str(s):
            return s
        case bool(b):
            return when b use "yes" otherwise "no"

function main() -> int32:
    let n: Number = 2.5            // holds a float64
    print(n, n is float64, n is int)
    print(n as float64)

    n = 7                          // now holds an int
    print(n, n is int, (n as int) + 1)

    let values: [4; Value] = {42, "hello", true, 3}
    for v in values:
        print(v, describe(v))

    let x = 5 as Number
    print(x)

    print("about to read the wrong type")
    n = 1.5
    print(n as int)                // stops the program: n holds a float64
    print("never printed")
    return 0

// expect:
// 2.5 true false
// 2.5
// 7 true 8
// 42 big number
// hello hello
// true yes
// 3 small number
// 5
// about to read the wrong type
// expect-exit: -6
