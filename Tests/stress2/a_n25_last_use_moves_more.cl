// N25 / N26: a last use followed by `return` moves (even in a loop, or with uses of the variable on other paths);
// `let u = q` inside `if q:` moves the value out of the optional. None of these copy (checked with --copies)
// flags: --copies
// expect-no-warning: E103

let live = 0

class Blob:
    id: int

    function init(self, id: int):
        self.id = id
        live += 1

    operator copy(self) -> Blob:
        return Blob(self.id + 100)

    operator destruct(self):
        live -= 1

class W:
    items: List[int]

function build(n: int64) -> W:
    let l = List[int]()
    while true:
        l.push(1)
        if len(l) > n:
            return W(l)

function put(xs: *List[String], value: String, front: bool):
    if front:
        xs[0] = value
        return
    xs.push(value)

function find(n: int) -> ?Blob:
    if n > 0:
        return Blob(n)
    return none

function first(n: int) -> int:
    let q = find(n)
    if q:
        let u = q
        return u.id
    return 0

function keep(xs: *List[Blob], n: int):
    let b = Blob(n)
    for i in 0..3:
        if i == n:
            xs.push(b)
            return
    print("not kept", b.id)

function run():
    print(len(build(3).items))
    let xs = List[String]()
    xs.push(String("a"))
    put(&xs, String("b"), true)
    put(&xs, String("c"), false)
    print(xs)
    print(first(5), first(0))
    let blobs = List[Blob]()
    keep(&blobs, 1)
    keep(&blobs, 7)
    print(blobs[0].id, live)

function main() -> int32:
    run()
    print("live", live)
    return 0

// expect:
// 4
// [b, c]
// 5 0
// not kept 7
// 1 1
// live 0
