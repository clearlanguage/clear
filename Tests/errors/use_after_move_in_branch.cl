import "string"

function take(s: String):
    print(s)

function main(argc: int32) -> int32:
    let a = String("hi")
    if argc > 0:
        take(a)            // moved on one path
    print(a)               // ...so it may be empty here
    return 0

// expect-error: This variable was moved
