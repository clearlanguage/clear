enum Pair:
    Both(a: int, b: int)

function main() -> int32:
    switch Pair.Both(1, 2):
        case Both(x):
            print(x)
    return 0

// expect-error
