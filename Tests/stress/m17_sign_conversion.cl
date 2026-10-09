// stress test M17_sign_conversion
// expect-error: E048


function main() -> int32:
    let a: int8 = -1
    let b: uint8 = a
    return 0
