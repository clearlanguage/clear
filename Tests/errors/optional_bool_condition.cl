function main() -> int32:
    let flag: ?bool = false
    if flag:                 // has a value, or is true? ambiguous
        print("yes")
    return 0

// expect-error: An optional bool is ambiguous
