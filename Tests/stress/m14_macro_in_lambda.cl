// stress test M14_macro_in_lambda
// expect:
// 30


macro twice(x):
    x * 2
function main() -> int32:
    let k = 10
    let f = lambda (a: int): twice!(a + k)
    print(f(5))
    return 0
