// stress test M5_const_array_param
// expect:
// 1 8


const N = 8
function first(a: [N; int]) -> int:
    return a[0]
class Buf:
    data: [N; int]
function main() -> int32:
    let a: [N; int] = {1, 2, 3, 4, 5, 6, 7, 8}
    let b = Buf { }
    print(first(a), len(b.data))
    return 0
