async function add(a: int, b: int) -> int:
    return a + b

async function worker(name: str, steps: int) -> int:
    for i in 0..steps:
        print(name, "step", i)
        await pause()                        // let other tasks run
    return await add(steps, 100)             // wait for another task

async function main_task() -> int:
    let a = await worker("solo", 2)
    return a

function main() -> int32:
    print(add(2, 3).run())                   // run a task to the end from ordinary code
    print(main_task().run())

    // interleave two tasks: a tiny round-robin scheduler
    let first = worker("x", 2)
    let second = worker("y", 2)
    while not (first.done() and second.done()):
        first.resume()
        second.resume()
    print(first.result(), second.result())
    first.free()
    second.free()
    return 0

// expect:
// 5
// solo step 0
// solo step 1
// 102
// x step 0
// y step 0
// x step 1
// y step 1
// 102 102
