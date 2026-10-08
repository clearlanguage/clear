import "map"
import "string"

class Stats:
    count: int
    total: float64

function main() -> int32:
    let m = Map[str, Stats]()
    defer m.free()
    let data: [6; (str, float64)] = {("a", 1.0), ("b", 2.0), ("a", 3.0), ("c", 4.0), ("b", 6.0), ("a", 5.0)}
    for pair in data:
        let key = pair[0]
        let s = m.get_or(key, Stats(0, 0.0))
        s.count += 1
        s.total += pair[1]
        m[key] = s
    print(m["a"].count, m["a"].total, m["b"].total / m["b"].count, len(m))
    let words = Map[String, int]()
    words[String("hi")] = 1
    words[String("hi")] += 1
    print(words[String("hi")])
    return 0

// expect:
// 3 9.0 4.0 3
// 2
