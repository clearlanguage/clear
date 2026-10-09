// N19: `*p = v` (and p[i] = v) cleans up the old value like any other assignment;
// place(p, v) fills raw memory that holds no value yet

import "memory"

class Tracked:
    id: int

    operator destruct(self):
        print("  destruct", self.id)

function replace(p: *String, text: str):
    *p = String(text)

function main() -> int32:
    let a = String("old value")
    let p = &a
    *p = String("new")
    print(a)
    replace(&a, "newer")
    print(a)

    let l = List[int]()
    l.push(1)
    let m = List[int]()
    m.push(2)
    let q = &l
    *q = m
    print(len(l), l[0])

    let t = Tracked(1)
    let tp = &t
    print("assign through pointer")
    *tp = Tracked(2)
    print("after", t.id)

    let raw = allocate[String](2)
    place(raw, String("first"))
    place(raw + 1, String("second"))
    *(raw + 1) = String("replaced")
    *raw = String("again")
    print(*raw, *(raw + 1))
    destroy(raw)
    destroy(raw + 1)
    release(raw)

    let cells = allocate[Tracked](1)
    place(cells, Tracked(3))
    print("replace raw cell")
    *cells = Tracked(4)
    destroy(cells)
    release(cells)
    print("end")
    return 0

// expect:
// new
// newer
// 1 2
// assign through pointer
//   destruct 1
// after 2
// again replaced
// replace raw cell
//   destruct 3
//   destruct 4
// end
//   destruct 2
