enum Light:
    Red
    Amber
    Green

function main() -> int32:
    let l = Light.Red
    switch l:
        case Light.Red:
            print("stop")
        case Light.Green:
            print("go")
    return 0

// expect-error
