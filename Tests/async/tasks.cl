async function add(a: int, b: int) -> int:
    return a + b

async function sum_to(n: int) -> int:
    let total = 0
    for i in 1..=n:
        total += await add(total, i) - total    // each step is its own task
    return total

// a task that gives way after each step, so others can run in between
async function worker(name: str, steps: int) -> int:
    for i in 0..steps:
        print(name, "step", i)
        await pause()
    return steps * 10

async function log(message: str):
    print("log:", message)

async function pipeline() -> int:
    await log("start")
    let a = await worker("a", 2)
    let b = await add(a, 1)
    await log("end")
    return b

function main() -> int32:
    print(add(2, 3).run())
    print(sum_to(10).run())
    print(pipeline().run())

    // two tasks interleaved by hand: a tiny round-robin scheduler
    let first = worker("x", 3)
    let second = worker("y", 2)
    let running = 2
    while running > 0:
        running = 0
        if not first.done() and not first.resume():
            running += 1
        if not second.done() and not second.resume():
            running += 1
    print(first.result(), second.result())
    first.free()
    second.free()
    return 0

// expect:
// 5
// 55
// log: start
// a step 0
// a step 1
// log: end
// 21
// x step 0
// y step 0
// x step 1
// y step 1
// x step 2
// 30 20
