// stress test H3_unsigned
// expect:
// 57005 9 true
// 2000000000


function main() -> int32:
    let c: uint32 = 0xDEADBEEF
    print(c >> 16, c % 10, c > 0)
    let m: uint32 = 4000000000
    print(m / 2)
    return 0
