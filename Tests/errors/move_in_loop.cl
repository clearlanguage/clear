import "string"
import "list"

function main() -> int32:
    let xs = List[String]()
    let s = String("x")
    for i in 0..3:
        xs.push(s)         // the second time round, s is already empty
    return 0

// expect-error: the next time round the loop uses again
