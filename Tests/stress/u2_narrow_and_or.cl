// stress test U2_narrow_and_or
// expect:
// 3


function f(x: ?int, y: ?int) -> int:
    if not x or not y:
        return -1
    return x + y
function main() -> int32:
    print(f(1, 2))
    return 0
