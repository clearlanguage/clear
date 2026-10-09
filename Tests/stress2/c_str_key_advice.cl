// a str key on a Map[String, V] suggests String(key) (N43)
function main() -> int32:
    let m = Map[String, int]()
    let a: str = "ada"
    m[String(a)] = 1
    print(m[a])
    return 0

// expect-error: write String(x) to make one from a str
