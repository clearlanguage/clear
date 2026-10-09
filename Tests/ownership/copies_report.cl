// flags: --copies
import "string"

function main() -> int32:
    let a = String("text")
    let b = a                // a is used again below: a real copy, reported
    b.append("!")
    print(a, b)
    return 0

// expect-warning: A copy is made here
// expect:
// text text!
