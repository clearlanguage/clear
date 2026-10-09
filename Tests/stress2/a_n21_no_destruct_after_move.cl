// N21: a value that was moved out of a variable is not cleaned up there: its operator destruct runs once,
// for the owner it ended up with (a conditional move uses a flag). Counting destructors balance.

let live = 0

class Blob:
    id: int

    function init(self, id: int):
        self.id = id
        live += 1

    operator destruct(self):
        print("  destruct", self.id)
        live -= 1

class Counted:
    id: int

    function init(self, id: int):
        self.id = id
        live += 1

    operator copy(self) -> Counted:
        return Counted(self.id + 100)

    operator destruct(self):
        live -= 1

function keep(xs: *List[Blob], b: Blob):
    xs.push(b)

function make(id: int) -> Blob:
    let b = Blob(id)
    return b

function consume(c: Counted) -> int:
    return c.id

function maybe(flag: bool) -> int:
    let c = Counted(7)
    if flag:
        return consume(c)
    return 0

function run():
    let a = Blob(1)
    let b = a
    print("moved local")
    let xs = List[Blob]()
    xs.push(Blob(2))
    print("pushed temp")
    keep(&xs, Blob(3))
    print("kept via fn")
    let c = Blob(4)
    xs.push(c)
    print("pushed local")
    let d = make(5)
    print("made", d.id)
    let e = Blob(6)
    if d.id == 5:
        xs.push(e)
    print("moved in a branch")
    let f = Blob(8)
    if d.id == 0:
        xs.push(f)
    print("not moved in a branch")
    let g = Blob(9)
    xs.push(g)
    g = Blob(10)
    print("given a new value", g.id)
    xs.clear()
    print("cleared, live", live)
    let total = 0
    for i in 0..3:
        let k = Counted(i)
        if i % 2 == 0:
            total += consume(k)
    print(total, maybe(true), maybe(false), live)

function main() -> int32:
    run()
    print("live at the end", live)
    return 0

// expect:
// moved local
// pushed temp
// kept via fn
// pushed local
// made 5
// moved in a branch
// not moved in a branch
// given a new value 10
//   destruct 2
//   destruct 3
//   destruct 4
//   destruct 6
//   destruct 9
// cleared, live 4
// 2 7 0 4
//   destruct 10
//   destruct 8
//   destruct 5
//   destruct 1
// live at the end 0
