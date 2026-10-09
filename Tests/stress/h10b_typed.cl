// stress test H10b_typed
// expect:
// 4


import "h10b_modp"
function main() -> int32:
    let p: List[int] = make()
    print(p[0])
    return 0
