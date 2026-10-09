// N22: a hex literal above 64 bits is an error instead of silently wrapping (it printed 1)
function main() -> int32:
    let u: uint64 = 0x1_0000_0000_0000_0000
    print(u)
    return 0

// expect-error: ‘0x1_0000_0000_0000_0000’ does not fit in 64 bits
