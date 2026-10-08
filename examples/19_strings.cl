import "string"

function main() -> int32:
    let s = String("hello")            // an owned, growable string
    defer s.free()                     // you decide when its memory goes back
    s.append(", world")
    s.push('!')
    print(s, len(s), s.find("world"))

    print(s.starts_with("hell"), s.ends_with("!"), "world" in s)

    let shout = s.upper()
    print(shout, s.slice(7, 12))
    shout.free()

    let a = String("abc")
    let b = a + String("def")          // + makes a new String
    print(b, b == String("abcdef"), a < b)

    let n = from_int(42)
    n.append(" is the answer")
    print(n, String("  padded  ").strip(), String("17").to_int() + 1)

    let plain: str = "literal"         // str: a C string literal, no allocation
    print(plain, len(plain), plain == "literal")
    return 0

// expect:
// hello, world! 13 7
// true true true
// HELLO, WORLD! world
// abcdef true true
// 42 is the answer padded 18
// literal 7 true
