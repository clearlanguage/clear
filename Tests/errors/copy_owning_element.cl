import "list"
import "string"

function main() -> int32:
    let names = List[String]()
    names.push(String("ada"))
    let first = names[0]     // would be a second owner of the same text
    return 0

// expect-error
