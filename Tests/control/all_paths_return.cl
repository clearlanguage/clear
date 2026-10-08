function classify(x: int) -> int:
    if x > 0:
        return 1
    else:
        return 0

function forever() -> int:
    let i = 0
    while true:
        i += 1
        if i == 10:
            return i

function pick(x: int) -> int:
    switch x:
        case 1:
            return 10
        default:
            return 20

function main() -> int32:
    print(classify(5), forever(), pick(1), pick(3))
    return 0

// expect:
// 1 10 10 20
