// stress test M11_macro_value
// expect-error: E106


macro twice(s):
    let out = String("")
    out.append(s)
function main() -> int32:
    let r = twice!("ab")
    return 0
