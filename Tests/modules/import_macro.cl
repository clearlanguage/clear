import "lib/checks"

function main() -> int32:
    let n = 5
    check!(n > 3, "n is big")
    check!(n > 30, "n is huge")
    return 0

// expect:
// check failed: n is huge
