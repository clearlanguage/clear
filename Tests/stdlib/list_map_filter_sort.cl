// List.map / filter / sort / sort_by (generic methods of a generic class)
import "list"
import "string"
function double(x: int) -> int:
    return x * 2
function main() -> int32:
    let xs = List[int]()
    let raw: [5; int] = {5, 3, 9, 1, 7}
    for i in raw:
        xs.push(i)
    let ys = xs.map(lambda x: x * 10)
    print(ys[0], ys[1], ys[4], len(ys))
    let zs = xs.map(double)
    print(zs[0])
    let big = xs.filter(lambda x: x > 4)
    print(len(big), big[0], big[1], big[2])
    let limit = 6
    let small = xs.filter(lambda x: x < limit)
    print(len(small))
    xs.sort()
    print(xs[0], xs[1], xs[2], xs[3], xs[4])
    let words = List[String]()
    let fruit: [5; str] = {"pear", "fig", "banana", "kiwi", "apple"}
    for w in fruit:
        words.push(String(w))
    words.sort()
    print(words[0], words[1], words[2], words[3], words[4])
    words.sort_by(lambda w: len(w))
    print(words[0], words[1], words[2], words[3], words[4])
    let lengths = words.map(lambda w: len(w))
    print(lengths[0], lengths[4])
    let shouts = words.map(lambda w: w.upper())
    print(shouts[0], words[0])
    let half = xs.map(lambda x: x as float64 / 2.0)
    print(half[1])
    return 0

// expect:
// 50 30 70 5
// 10
// 3 5 9 7
// 3
// 1 3 5 7 9
// apple banana fig kiwi pear
// fig kiwi pear apple banana
// 3 6
// FIG fig
// 1.5
