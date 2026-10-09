import "list"

function main() -> int32:
    let numbers = List[int]()
    numbers.push(1)
    numbers.push(2)
    let part = numbers[0:2]
    print(part[0])
    numbers.push(3)          // may move the items part looks at
    print(len(numbers))
    print(part[1] > 0)
    return 0

// expect-warning: ‘part’ points into ‘numbers’
// expect:
// 1
// 3
// true
