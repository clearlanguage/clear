import "map"

enum Color:
    Red
    Green

function main() -> int32:
    let ages = Map[str, int]()
    defer ages.free()

    ages["ada"] = 36
    ages["alan"] = 41
    ages["grace"] = 85
    ages["ada"] = 37

    print(len(ages), ages["ada"], "alan" in ages, "bob" in ages, "bob" not in ages)
    print(ages.get_or("bob", -1), ages.get("grace"), ages.get("bob"))

    print(ages.remove("alan"), ages.remove("alan"), len(ages))

    let total = 0
    for name in ages:
        total += ages[name]
    print(total)

    // many integer keys: forces several resizes
    let squares = Map[int64, int64]()
    defer squares.free()
    for i in 0..1000:
        squares[i] = i * i
    for i in 0..500:
        squares.remove(i * 2)
    let sum: int64 = 0
    for key in squares:
        sum += squares[key]
    print(len(squares), squares[999], sum)

    let counts = Map[Color, int]()
    counts[Color.Red] = 1
    counts[Color.Red] += 2
    print(counts[Color.Red], Color.Green in counts)
    counts.free()
    return 0

// expect:
// 3 37 true false true
// -1 85 none
// true false 2
// 122
// 500 998001 166666500
// 3 false
