// stress test U6_infer
// expect:
// 4
// 3


function apply_to[T, U](x: T, f: function(T) -> U) -> U:
    return f(x)
function count[T](xs: []T) -> int64:
    return len(xs)
function main() -> int32:
    print(apply_to(2, lambda x: x * 2))
    let words = List[String]()
    words.push(String("a"))
    words.push(String("b"))
    words.push(String("c"))
    print(count(words))
    return 0
