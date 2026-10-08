class Bag:
    count: int

function main() -> int32:
    let bag = Bag(3)
    for item in bag:
        print(item)
    return 0

// expect-error
