function main() -> int32:
    let x = 1
    let y = 2
    switch x:
        case y:
            print("y")
    return 0

// expect-error
