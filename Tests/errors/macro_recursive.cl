macro forever(x):
    forever!(x)

function main() -> int32:
    forever!(1)
    return 0

// expect-error
