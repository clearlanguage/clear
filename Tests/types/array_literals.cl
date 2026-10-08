const SIZE = 5

function main() -> int32:
    let zeros: [SIZE; int] = {}
    let partial: [4; float64] = {1, 2.5}
    let bytes: [3; uint8] = {255, 1}
    let f: float = 0.1
    print(zeros, partial, bytes, sizeof f as int)
    return 0

// expect:
// [0, 0, 0, 0, 0] [1.0, 2.5, 0.0, 0.0] [255, 1, 0] 8
