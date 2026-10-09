import "list"

function main() -> int32:
    let xs = List[int]()
    xs.push(1)
    let first = &xs[0]
    xs.push(2)              // may move every item to a bigger block of memory
    first = &xs[0]          // taking the pointer again is fine...
    print(*first)
    let second = &xs[1]
    xs.push(3)
    print(*second >= 0)     // ...using the old one is warned about
    return 0

// expect-warning: ‘second’ points into ‘xs’, which was changed by ‘push’
// expect:
// 1
// true
