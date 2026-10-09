// a copy out of a local becomes a move at its last use (not in loops, defers, or when a pointer looks into it)
import "string"
import "list"
class Counted:
    n: int
    operator copy(self) -> Counted:
        print("  copy of", self.n)
        return Counted(self.n)
function keep(c: Counted):
    print("  kept", c.n)
function main() -> int32:
    print("last use: no copy")
    let a = Counted(1)
    keep(a)
    print("used again after: copy")
    let b = Counted(2)
    keep(b)
    print("b still", b.n)
    print("in a loop: copy each time")
    let c = Counted(3)
    for i in 0..2:
        keep(c)
    print("declared in the loop: no copy")
    for i in 0..2:
        let d = Counted(4)
        keep(d)
    print("twice in one call: copy")
    let e = Counted(5)
    let pair = (e, e)
    print("deferred use: copy")
    let f = Counted(6)
    defer print("  defer sees", f.n)
    keep(f)
    print("pointer kept: copy")
    let g = Counted(7)
    let p = &g
    keep(g)
    print("  through p", p.n)
    print("one branch")
    let h = Counted(8)
    if len("x") == 1:
        keep(h)
    else:
        print("  other")
    let names = List[String]()
    let s = String("moved into the list")
    names.push(s)
    print(names[0])
    return 0

// expect:
// last use: no copy
//   kept 1
// used again after: copy
//   copy of 2
//   kept 2
// b still 2
// in a loop: copy each time
//   copy of 3
//   kept 3
//   copy of 3
//   kept 3
// declared in the loop: no copy
//   kept 4
//   kept 4
// twice in one call: copy
//   copy of 5
// deferred use: copy
//   copy of 6
//   kept 6
// pointer kept: copy
//   copy of 7
//   kept 7
//   through p 7
// one branch
//   kept 8
// moved into the list
//   defer sees 6
