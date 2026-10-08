declare printf(fmt: *int8, args: ...) -> int32

function main() -> int32:
    let c = 'a'
    let t = true
    let x = 0xFF
    let y = 0b101
    let big = 5000000000
    printf("%c %d %d %d %d %lld\n", c, c, t, x, y, big)
    return 0

// expect:
// a 97 1 255 5 5000000000
