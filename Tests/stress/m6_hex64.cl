// stress test M6_hex64
// expect:
// 0 9223372036854775808


function main() -> int32:
    let w: uint64 = 0xFFFFFFFFFFFFFFFF
    let x = w ^ 0xFFFFFFFFFFFFFFFF
    let m: uint64 = 0x8000000000000000
    print(x, m)
    return 0
