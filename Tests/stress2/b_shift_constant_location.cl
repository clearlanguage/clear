// N20: a constant shift amount out of range is E064 pointing at the amount (it had no location)
function main() -> int32:
    let x: int64 = 1
    print(x << 64)
    return 0

// expect-error: b_shift_constant_location.cl:4:16
