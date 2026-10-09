function main() -> int32:
    let n = 5
    print(n ?? 0)            // n is always there
    return 0

// expect-error: This is not an optional
