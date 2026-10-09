// stress test L4_task_string_leak
// expect:
// result
// done


async function work() -> String:
    await pause()
    return String("result")
function main() -> int32:
    let t = work()
    while not t.done():
        t.resume()
    print(t.result())
    let u = work()
    while not u.done():
        u.resume()
    print("done")
    return 0
