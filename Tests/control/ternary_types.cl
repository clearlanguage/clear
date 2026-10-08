function main() -> int32:
    let big: int64 = 5000000000
    let small = 3
    let a = when small > 1 use small otherwise big
    let b = when small > 1 use 8 otherwise big * 2
    let c = when small > 5 use 1.5 otherwise small
    let d = -2.5
    print(a, b, c, -d, sizeof a as int)
    return 0

// expect:
// 3 8 3.0 2.5 8
