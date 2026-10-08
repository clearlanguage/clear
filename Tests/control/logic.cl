declare printf(fmt: *int8, args: ...) -> int32

function main() -> int32:
    let a = 3
    if a > 1 and a < 5:
        printf("and\n")
    if a == 2 or a == 3:
        printf("or\n")
    if not (a == 4) and a != 7:
        printf("not\n")
    return 0

// expect:
// and
// or
// not
