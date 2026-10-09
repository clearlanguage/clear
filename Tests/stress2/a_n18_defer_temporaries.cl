// N18: a deferred expression's temporaries are cleaned up, and it is computed afresh at every exit
// (also in an async function after await)

function work(id: int, early: bool) -> int:
    defer print("done " + from_int(id))
    if early:
        return 1
    print("body", id)
    return 0

async function worker(id: int) -> int:
    defer print("finished " + from_int(id) + "!")
    await pause()
    if id > 5:
        return id * 2
    return id

function main() -> int32:
    let id = 7
    defer print("main " + from_int(id))
    print(work(1, true), work(2, false))
    for i in 0..3:
        defer print("step " + from_int(i))
        if i == 1:
            continue
        print("i", i)
    print(worker(7).run(), worker(3).run())
    return 0

// expect:
// done 1
// body 2
// done 2
// 1 0
// i 0
// step 0
// step 1
// i 2
// step 2
// finished 7!
// finished 3!
// 14 3
// main 7
