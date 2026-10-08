declare printf(fmt: *int8, args: ...) -> int32

function main() -> int32:
    let f = 2.5
    let g = f * 2
    printf("%.2f %.2f\n", f, g)
    return 0

// expect:
// 2.50 5.00
