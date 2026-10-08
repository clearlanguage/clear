macro square(x):
    x * x

function main() -> int32:
    return square!(1, 2)

// expect-error
