// N27: swapping owning values with `a, b = b, a` moves them, it makes no copies (checked with --copies:
// a copy would print note E103, and the swap lines below must not)
// flags: --copies
// expect-no-warning: E103

let live = 0

class Blob:
    id: int

    function init(self, id: int):
        self.id = id
        live += 1

    operator copy(self) -> Blob:
        return Blob(self.id)

    operator destruct(self):
        live -= 1

function swap_tuple(xs: []String, i: int64, j: int64):
    xs[i], xs[j] = xs[j], xs[i]

function rotate(xs: *List[Blob]):
    xs[0], xs[1], xs[2] = xs[1], xs[2], xs[0]

function run():
    let w = List[String]()
    w.push(String("a"))
    w.push(String("b"))
    swap_tuple(w[:], 0, 1)
    print(w[0], w[1])
    swap_tuple(w[:], 1, 1)
    print(w[0], w[1])
    let a = String("first")
    let b = String("second")
    a, b = b, a
    print(a, b)
    let blobs = List[Blob]()
    for i in 0..3:
        blobs.push(Blob(i))
    rotate(&blobs)
    print(blobs[0].id, blobs[1].id, blobs[2].id, live)
    let n = 1
    let m = 2
    n, m = m, n
    print(n, m)

function main() -> int32:
    run()
    print("live", live)
    return 0

// expect:
// b a
// b a
// second first
// 1 2 0 3
// 2 1
// live 0
