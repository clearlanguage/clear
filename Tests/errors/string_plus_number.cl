function main() -> int32:
    let s = String("a")
    let t = s + 2            // a number is not text: write from_int(2)
    return 0

// expect-error
