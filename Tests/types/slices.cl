// slices: views of arrays, lists and other slices; []T parameters take any of them
import "list"
import "string"
function sum(values: []int) -> int:
    let total = 0
    for v in values:
        total += v
    return total
function first_two[T](values: []T) -> []T:
    return values[:2]
function main() -> int32:
    let xs: [6; int] = {10, 20, 30, 40, 50, 60}
    let part = xs[1:4]
    print(len(part), part[0], part[2])
    part[0] = 99
    print(xs[1])
    print(sum(xs), sum(xs[2:5]), sum(xs[:2]), sum(xs[4:]), sum(xs[:]))
    let smaller = part[1:]
    print(len(smaller), smaller[0])
    let numbers = List[int]()
    for i in 1..=10:
        numbers.push(i)
    print(sum(numbers), sum(numbers[:3]), sum(numbers[7:]))
    let view = numbers[2:5]
    view[0] = 300
    print(numbers[2])
    let firsts = first_two(xs[3:])
    print(firsts[0], firsts[1])
    let words = List[String]()
    words.push(String("a"))
    words.push(String("b"))
    words.push(String("c"))
    let tail = words[1:]
    for w in tail:
        w.append("!")
    print(words[0], words[1], words[2], len(tail))
    let copy = tail[0]
    copy.append("?")
    print(copy, words[1])
    return 0

// expect:
// 3 20 40
// 99
// 289 120 109 110 289
// 2 30
// 55 6 27
// 300
// 40 50
// a b! c! 2
// b!? b!
