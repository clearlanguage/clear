import "string"

function main() -> int32:
    let owned = String("text")
    let show = move lambda: print(owned)   // owned moves into the lambda
    show()
    print(owned)
    return 0

// expect-error: This variable was moved
