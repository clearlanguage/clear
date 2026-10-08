function main() -> int32:
    let arr: [3; int] = {1, 2, 3}
    let x = arr[1.5]
    return 0

// expect-error
