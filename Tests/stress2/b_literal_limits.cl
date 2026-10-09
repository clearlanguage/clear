// N16/N22: the largest literals that still fit are accepted, leading zeros don't count, floats may be bigger
function main() -> int32:
    let a: uint64 = 18446744073709551615
    let b: uint64 = 0xFFFF_FFFF_FFFF_FFFF
    let c: uint64 = 0x0000_0000_0000_0000_00FF
    let d: uint64 = 0b1111111111111111111111111111111111111111111111111111111111111111
    let e = 1e20
    print(a, b, c, d == a, e)
    return 0

// expect:
// 18446744073709551615 18446744073709551615 255 true 1e+20
