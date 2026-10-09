import "string"
import "list"
import "map"
class Tag:
    name: String
class Counted:
    n: int
    operator copy(self) -> Counted:
        print("copying", self.n)
        return Counted(self.n + 100)
function shout(s: String):
    s.append("!!")
    print("inside", s)
function main() -> int32:
    let a = String("self")
    a = a
    print("1", a)
    let words = List[String]()
    words.push(String("x"))
    words.push(String("y"))
    words[0] = words[1]
    words[1].append("2")
    print("2", words[0], words[1])
    for i in 0..20:
        words.push(words[0])
    print("3", len(words), words[21])
    let nested = List[List[String]]()
    nested.push(List[String]())
    nested[0].push(String("deep"))
    let copy = nested
    copy[0][0].append("er")
    print("4", nested[0][0], copy[0][0])
    let m = Map[str, String]()
    m["k"] = String("v")
    let m2 = m
    m2["k"].append("2")
    print("5", m["k"], m2["k"])
    let t1 = Tag(String("tag"))
    let t2 = t1
    t2.name.append("!")
    print("6", t1.name, t2.name)
    let msg = String("hi")
    shout(msg)
    print("7", msg)
    let c = Counted(1)
    let d = c                // changed below, so a real copy: operator copy runs
    d.n += 1
    print("8", c.n, d.n)
    let maybe: ?String = String("opt")
    let got = maybe.value
    got.append("?")
    print("9", maybe.value, got)
    return 0

// expect:
// 1 self
// 2 y y2
// 3 22 y
// 4 deep deeper
// 5 v v2
// 6 tag tag!
// inside hi!!
// 7 hi
// copying 1
// 8 1 102
// 9 opt opt?
