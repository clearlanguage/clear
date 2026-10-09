import "string"

function main() -> int32:
    let a = String("hello")
    let b = a              // a is moved into b and left empty
    print(a)               // so reading it is an error, not an empty string
    return 0

// expect-error: This variable was moved
