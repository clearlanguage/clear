function main() -> int32:
    let x: uint8 = 255
    x += 1
    let h: uint64 = 14695981039346656037
    h = h * 1099511628211
    print(x, h > 0)
    return 0

// flags: --checks
// expect:
// 0 true
