function divmod(a: int, b: int) -> (int, int):     // several results
    return a / b, a % b

function add3(a: int, b: int, c: int) -> int:
    return a + b + c

function main() -> int32:
    let t = (1, 2.5, "three")
    print(t, t[0], t[2])

    let q, r = divmod(17, 5)                        // destructuring
    print(q, r)

    let a = 1
    let b = 2
    a, b = b, a                                     // swap
    print(a, b)

    let values: [3; int] = {1, 2, 3}
    print(add3(values...))                          // spread an array into arguments
    let pair = (10, 20)
    print(add3(pair..., 30))                        // or a tuple
    return 0

// expect:
// (1, 2.5, three) 1 three
// 3 2
// 2 1
// 6
// 60
