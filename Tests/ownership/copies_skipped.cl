// copies are skipped where nobody could tell: a variable's last use moves, a value that is only read looks at the original
import "list"
import "string"
class Counted:
    n: int
    label: String
    operator copy(self) -> Counted:
        print("  copy of", self.n)
        return Counted(self.n, self.label.copy())
class Box:
    item: Counted
function main() -> int32:
    let items = List[Counted]()
    items.push(Counted(1, String("one")))
    items.push(Counted(2, String("two")))
    print("only read: no copy")
    for i in 0..len(items):
        let it = items[i]
        print("  ", it.n, it.label, len(it.label))
    print("changed: copy")
    let a = items[0]
    a.n = 10
    print("  ", a.n, items[0].n)
    print("list changes while in use: copy")
    let b = items[1]
    items.push(Counted(3, String("three")))
    print("  ", b.n)
    print("method call: copy")
    let c = items[0]
    c.label.append("!")
    print("  ", c.label, items[0].label)
    print("field of a local: no copy")
    let box = Box(Counted(4, String("four")))
    let inner = box.item
    print("  ", inner.n)
    print("returned value kept: copy")
    let words = List[String]()
    words.push(String("hello"))
    let w = words[0]
    words.clear()
    print("  ", w)
    return 0

// expect:
// only read: no copy
//    1 one 3
//    2 two 3
// changed: copy
//   copy of 1
//    10 1
// list changes while in use: copy
//   copy of 2
//    2
// method call: copy
//   copy of 1
//    one! one
// field of a local: no copy
//    4
// returned value kept: copy
//    hello
