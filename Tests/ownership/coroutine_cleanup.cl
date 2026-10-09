// generators and tasks clean up after themselves, including the values they hold while suspended
class Noisy:
    name: str

    operator destruct(self):
        print("destroy", self.name)

function numbers(label: str) -> Generator[int]:
    let held = Noisy(label)
    for i in 0..10:
        yield i

async function work(k: int) -> int:
    let held = Noisy("task")
    await pause()
    return k

async function outer() -> int:
    let t = work(5)
    let r = await t         // t is moved into the await
    return r + await work(1)

function peek():
    let g = numbers("peeked")
    g.advance()
    print("first value", g.value())

function start_only():
    let t = work(3)
    t.resume()
    print("task started")

function main() -> int32:
    for x in numbers("broken"):
        if x == 1:
            break           // the generator is abandoned while suspended
    print("after break")

    for x in numbers("finished"):
        if x == 9:
            print("last")
    print("after full loop")

    let g = numbers("kept")
    for x in g:
        if x == 1:
            break           // g is iterated in place, so it can carry on
    for x in g:
        if x == 3:
            print("carried on to", x)
            break
    g.free()
    print("freed")

    peek()
    start_only()
    print(work(7).run())
    print(outer().run())
    return 0

// expect:
// destroy broken
// after break
// last
// destroy finished
// after full loop
// carried on to 3
// destroy kept
// freed
// first value 0
// destroy peeked
// task started
// destroy task
// destroy task
// 7
// destroy task
// destroy task
// 6
