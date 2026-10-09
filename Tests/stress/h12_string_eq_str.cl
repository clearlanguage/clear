// stress test H12_string_eq_str
// expect:
// true false true


function main() -> int32:
    let s = String("ab")
    print(s == "ab", s != "ab", s < "b")
    return 0
