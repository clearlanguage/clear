// stress test H3_lcg
// flags: --checks
// expect:
// ok


function main() -> int32:
    let s: uint64 = 12345
    for i in 0..10:
        s = s * 6364136223846793005 + 1442695040888963407
    print("ok")
    return 0
