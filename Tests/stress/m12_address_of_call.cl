// stress test M12_address_of_call
// expect:
// 7


function seven() -> int:
    return 7
function show(p: *int):
    print(*p)
function main() -> int32:
    show(&seven())
    return 0
