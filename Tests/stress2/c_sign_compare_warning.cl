// comparing a signed with an unsigned integer warns: -1 is compared as a huge unsigned number (N46)
function main() -> int32:
    let u: uint32 = 5
    let i: int32 = -1
    print(u > i)
    return 0

// expect-warning: A signed and an unsigned integer are compared
// expect:
// false
