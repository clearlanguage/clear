enum Color:
    Red
    Green
    Blue = 10
    Purple

const LIMIT = 4
const SIZE = LIMIT * 2

function describe(c: Color) -> int:
    switch c:
        case Color.Red:
            return 1
        case Color.Green, Color.Blue:
            return 2
        default:
            return 3

function grade(score: int) -> int32:
    switch score / 10:
        case 10, 9:
            return 'A'
        case 8:
            return 'B'
        default:
            return 'F'

function cleanup(depth: int) -> int:
    defer print("leaving", depth)
    defer print("second defer runs first")
    if depth > 1:
        return depth * 10
    print("body", depth)
    return depth

function main() -> int32:
    let c = Color.Blue
    print(c, Color.Purple, describe(Color.Red), describe(c), describe(Color.Purple))
    print(grade(95), grade(81), grade(42))
    let buffer: [SIZE; int] = {1, 2, 3, 4, 5, 6, 7, 8}
    print(buffer, SIZE)
    print(cleanup(1))
    print(cleanup(2))
    for i in 0..3:
        defer print("end of iteration", i)
        if i == 1:
            continue
        print("iteration", i)
    print(Color.Blue as int + 1)
    return 0

// expect:
// Color.Blue Color.Purple 1 2 3
// 65 66 70
// [1, 2, 3, 4, 5, 6, 7, 8] 8
// body 1
// second defer runs first
// leaving 1
// 1
// second defer runs first
// leaving 2
// 20
// iteration 0
// end of iteration 0
// end of iteration 1
// iteration 2
// end of iteration 2
// 11
