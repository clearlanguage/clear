function find(n: int) -> ?int:
    return when n > 0 use n otherwise none

function main() -> int32:
    let r = find(3)
    if r:
        print(r + 1)         // fine: r holds a value here
    print(r + 1)             // r may be none here
    return 0

// expect-error
