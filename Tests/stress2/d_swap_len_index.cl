// a tuple swap whose indexes use len(xs) is still a swap of the same places: no copies
// flags: --copies
// expect-no-warning: E103

function main() -> int32:
    let xs = List[String]()
    xs.push(String("a"))
    xs.push(String("b"))
    xs.push(String("c"))
    let s = xs[:]
    s[len(s) - 1], s[0] = s[0], s[len(s) - 1]
    print(xs)
    return 0

// expect:
// [c, b, a]
