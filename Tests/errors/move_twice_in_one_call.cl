import "string"

function both(a: String, b: String):
    print(a, b)

function main() -> int32:
    let a = String("x")
    both(a, a)             // the second argument finds a already moved
    return 0

// expect-error: This variable was moved
