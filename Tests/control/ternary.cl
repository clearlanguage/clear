declare printf(fmt: *int8, args: ...) -> int32

function main() -> int32:
    let t = 50
    let m = when t > 40 use "big" otherwise "small"
    printf("%s\n", m)
    return 0

// expect:
// big
