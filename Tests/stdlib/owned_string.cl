import "string"
import "map"

function main() -> int32:
    let s = String("hello")
    defer s.free()
    s.append(", world")
    s.push('!')
    print(s, len(s), s[0], s.find("world"), s.find("xyz"))
    print("world" in s, s.starts_with("hell"), s.ends_with("!"), s.ends_with("?"))

    let upper = s.upper()
    print(upper, s.slice(7, 12))
    upper.free()

    let a = String("abc")
    let b = String("abc")
    let c = a + String("def")
    print(a == b, a != c, a < c, c, c.equals("abcdef"))

    let padded = String("  trim me \n")
    let stripped = padded.strip()
    print("[", stripped, "]")

    let number = from_int(-42)
    number.append(" and ")
    number.append_float(2.5)
    print(number)
    print(String("123").to_int() + 1)

    let counts = Map[String, int]()
    counts[String("x")] = 0
    for word in 0..3:
        counts[String("x")] += 1
    counts[String("y")] = 7
    print(len(counts), counts[String("x")], counts[String("y")])

    let empty: String
    print("[", empty, "]", len(empty))
    return 0

// expect:
// hello, world! 13 104 7 -1
// true true true false
// HELLO, WORLD! world
// true true true abcdef true
// [ trim me ]
// -42 and 2.5
// 124
// 2 3 7
// [  ] 0
