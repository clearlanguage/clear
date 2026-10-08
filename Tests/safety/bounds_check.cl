function main() -> int32:
    let arr: [3; int] = {1, 2, 3}
    let i = 5
    print("before")
    print(arr[i])
    return 0

// flags: --checks
// expect:
// before
// expect-exit: -6
