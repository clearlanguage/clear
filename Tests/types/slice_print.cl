// print shows a slice's items, and a String inside one as its text
import "list"
import "string"
function main() -> int32:
    let xs: [3; int] = {1, 2, 3}
    print(xs[1:], "and", xs[:0], xs[:])
    let fs: [2; float64] = {0.5, 2.0}
    print("floats", fs[:])
    let words = List[String]()
    words.push(String("a"))
    words.push(String("b"))
    let ws: []String = words
    print(ws, len(ws))
    return 0

// expect:
// [2, 3] and [] [1, 2, 3]
// floats [0.5, 2.0]
// [a, b] 2
