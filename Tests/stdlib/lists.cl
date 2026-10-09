import "list"

class Point:
    x: int
    y: int

function main() -> int32:
    let numbers = List[int]()
    defer numbers.free()

    for i in 0..20:
        numbers.push(i * i)

    numbers[0] = 100
    print(numbers.length, numbers[0], numbers[19], numbers.last(), numbers.contains(49), numbers.contains(50))
    print(numbers.pop(), numbers.length)

    let points = List[Point]()
    points.push(Point(1, 2))
    points.push(Point(3, 4))
    print(points[1], points.length)
    points.free()
    print(points.is_empty())
    return 0

// expect:
// 20 100 361 361 true false
// 361 19
// Point(x=3, y=4) 2
// true
