function main() -> int32:
    let f = lambda x: x + 1     // fine on its own: x gets its type from each call
    print(f(1, 2))              // ...but this call does not fit
    return 0

// expect-error: expects 1 argument
