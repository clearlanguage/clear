// stress test L2_optional_temp_leak
// expect:
// true false


function find(n: int) -> ?String:
    if n > 0:
        return String("x")
    return none
function main() -> int32:
    print(find(1) is not none, find(0) is not none)
    return 0
