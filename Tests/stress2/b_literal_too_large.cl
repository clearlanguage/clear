// N16: a decimal literal of 2^64 or more is a located error (it crashed the compiler)
function main() -> int32:
    let u = 99999999999999999999
    print(u)
    return 0

// expect-error: ‘99999999999999999999’ does not fit in 64 bits
