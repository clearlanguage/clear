// a `when` whose sides are text literals becomes a String where a String is expected, like one literal does

function show(b: bool) -> String:
    return when b use "yes" otherwise "no"

function grade(n: int) -> String:
    return when n > 8 use "high" otherwise when n > 4 use "mid" otherwise "low"

function main() -> int32:
    let s: String = when 1 > 2 use "one" otherwise "two"
    s += "!"
    print(show(true), show(false), s, grade(9), grade(5), grade(1))
    return 0

// expect:
// yes no two! high mid low
