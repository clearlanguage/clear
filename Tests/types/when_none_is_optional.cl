// `when c use value otherwise none` is an optional (found by the fuzzer)
function find(x: int) -> ?int:
    return when x > 2 use x * 10 otherwise none

function main() -> int32:
    let a: ?int = when 1 < 2 use 5 otherwise none
    let b = when 1 > 2 use 5 otherwise none
    let c: ?int64 = when true use 7 otherwise none
    print(a.value_or(-1), b is none, c.value_or(0), find(3).value_or(-1), find(1) is none)
    return 0

// expect:
// 5 true 7 30 true
