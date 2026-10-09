// N3 / N8: tuple elements that own memory, read with len, [], &, methods and fields; copying such a tuple

import "memory"

class Buf:
    data: *int8
    size: int64

    function init(self, n: int64):
        self.data = allocate[int8](n)
        self.size = n
        for i in 0..n:
            *(self.data + i) = 7

    operator len(self) -> int64:
        return self.size

    operator destruct(self):
        if self.data != null:
            release(self.data)

    operator copy(self) -> Buf:
        return Buf(self.size)

class Named:
    name: String

function main() -> int32:
    let t = (String("abc"), 5)
    print(len(t[0]), t[1])
    let p = &t[0]
    print(len(*p), *p)

    let l = List[int]()
    l.push(7)
    let u = (l, 5)
    print(u[0].is_empty(), u[0][0], u[0].contains(7), len(u[0]))
    u[0].push(8)
    u[0][0] = 9
    print(u[0])

    let b = (Buf(3), Named(String("n")))
    print(len(b[0]), b[1].name, len(b[1].name))

    let c = (List[int](), 5)
    c[0].push(1)
    let d = c
    d[0].push(2)
    print(len(d[0]), len(c[0]), d[1])

    let e = (String("x"), Buf(2))
    let f = e
    print(f[0], len(f[1]), e[0], len(e[1]))
    return 0

// expect:
// 3 5
// 3 abc
// false 7 true 1
// [9, 8]
// 3 n 1
// 2 1 5
// x 2 x 2
