// N20: without checks a shift by the bit width or more uses the amount modulo the width (the same at every -O level)
// flags: --no-checks
function rotl32(x: uint32, r: uint32) -> uint32:
    return (x << r) | (x >> (32 - r))

function main() -> int32:
    let x: uint32 = 0xDEADBEEF
    let s: uint32 = 32
    let n = 70
    let one: int64 = 1
    let minus: int32 = -64
    let k: int8 = 9
    let small: uint8 = 0x81
    print(x >> s, rotl32(x, 0) == x, rotl32(x, 8))
    let t = 33
    print(one << n, minus >> t, small << k, small >> k)
    let y: int64 = 5
    y <<= 65
    print(y)
    return 0

// expect:
// 3735928559 true 2914971614
// 64 -32 2 64
// 10
