// N17: array literals of owning values must not leak (each item is owned by the array, nothing is copied twice)

enum R:
    Set(key: String)
    Clear

function total(items: [2; String]) -> int64:
    return len(items[0]) + len(items[1])

function main() -> int32:
    let arr = [String("x"), String("yy")]
    print(arr[0], arr[1])
    let b: [2; String] = {String("x"), String("y")}
    print(b[1])
    let x = String("kept")
    let c: [2; String] = {x, String("y")}
    print(c[0], x)
    let y = String("moved")
    let d = [y, String("z")]
    print(d[0], d[1])
    let e: [3; String] = {String("only")}
    e[2] = String("set")
    print(e[0], len(e[1]), e[2])
    let r: [1; R] = {R.Set(key = String("k"))}
    print(len(r))
    print(total([String("ab"), String("cde")]))
    let sum: int64 = 0
    for i in 0..3:
        let pair = [String("p"), from_int(i)]
        sum += len(pair[0]) + len(pair[1])
    print(sum)
    let kept = List[String]()
    for s in [String("a"), String("b")]:
        print(s)
        kept.push(s)
    print(kept)
    return 0

// expect:
// x yy
// y
// kept kept
// moved z
// only 0 set
// 1
// 5
// 6
// a
// b
// [a, b]
