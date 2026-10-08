let scale = 10

function scaled[T](x: T) -> T:
    // must see the global `scale`, never a local of whoever calls it
    return x * scale

function main() -> int32:
    let scale = 1000
    print(scaled(3), scale)
    return 0

// expect:
// 30 1000
