function lookup(m: *Map[String, int], key: str) -> int:
    return m[String(key)]

function main() -> int32:
    let m = Map[String, int]()
    m[String("a")] = 1
    print(lookup(&m, "a"))
    print(lookup(&m, "b"))
    return 0

// flags: --checks
// expect:
// 1
// expect-exit: -6
// expect-stderr: panic: assertion failed (c_d6_map_key.cl:2:12): key not in Map
