// a variant holds a value of one of its types and remembers which one
variant Number:
    int
    float64

variant Setting:
    bool
    int
    str

function describe(s: Setting) -> str:
    switch s:                          // every type must be handled
        case bool(on):
            return when on use "on" otherwise "off"
        case int(n):
            return when n > 100 use "large" otherwise "small"
        case str(text):
            return text

function main() -> int32:
    let n: Number = 2.5                // holds a float64
    print(n, n is float64, n is int)
    print(n as float64 + 1.0)          // read it as the type it holds

    n = 7                              // now holds an int
    print(n, (n as int) * 2)

    let settings: [3; Setting] = {true, 250, "custom"}
    for s in settings:
        print(s, "->", describe(s))

    // reading the wrong type stops the program instead of returning garbage:
    //     print(n as float64)   ->   panic: reading float64 from a Number that holds another type
    return 0

// expect:
// 2.5 true false
// 3.5
// 7 14
// true -> on
// 250 -> large
// custom -> custom
