// stress test H9_task_list
// expect:
// 2 1


async function agent(n: int) -> int:
    await pause()
    return n
function main() -> int32:
    let tasks = List[Task[int]]()
    tasks.push(agent(2))
    tasks.push(agent(1))
    for t in tasks:
        while not t.done():
            t.resume()
    print(tasks[0].result(), tasks[1].result())
    return 0
