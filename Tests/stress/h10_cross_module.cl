// stress test H10_cross_module
// expect:
// 1 false


import "h10_modp"
function main() -> int32:
    print(len(make()), make().is_empty())
    return 0
