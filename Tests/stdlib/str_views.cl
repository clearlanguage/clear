// str is a view of text: literals, Strings and parts of them (text[a:b]) without copying
import "string"
import "map"
import "io"
function shout(text: str):
    print(text, "!")
function main() -> int32:
    let hello: str = "hello world"
    print(hello, len(hello))
    let word = hello[0:5]
    print(word, len(word), word == "hello", word < "help", hello[6:])
    let name = String("ada lovelace")
    shout("literal")
    shout(name)
    shout(name[4:])
    let first = name[:3]
    print(first, first == "ada", "love" in name, "xyz" in hello, "lo w" in hello)
    print(name.find("love"), name.starts_with("ada"), name.ends_with("lace"), name.ends_with("ada"))
    for c in word:
        print(c)
        break
    let ages = Map[str, int]()
    ages["ada"] = 36
    ages["alan"] = 41
    print(ages["ada"], ages.get_or("bob", -1), len(ages))
    let copy = String(word)
    copy.append("!")
    print(copy, word)
    print(hash("abc") == hash(name[0:0]), hash("ab") == hash(hello[0:0]), name.to_int())
    let n = String("42")
    print(n.to_int() + 1)
    write_file("scratch_str.txt", hello[6:])
    let back = read_file("scratch_str.txt")
    print(back ?? String("?"))
    delete_file("scratch_str.txt")
    return 0

// expect:
// hello world 11
// hello 5 true true world
// literal !
// ada lovelace !
// lovelace !
// ada true true false true
// 4 true true false
// 104
// 36 -1 2
// hello! hello
// false false 0
// 43
// world
