import "list"

class Countdown:
    start: int

    operator len(self: *Countdown) -> int:
        return self.start

    operator get(self: *Countdown, i: int) -> int:
        return self.start - i

function make() -> List[float64]:
    let values = List[float64]()
    values.push(0.5)
    values.push(1.5)
    return values

function main() -> int32:
    let numbers = List[int]()
    defer numbers.free()
    for i in 1..=5:
        numbers.push(i * 10)

    let total = 0
    for n in numbers:
        if n == 30:
            continue
        total += n
    print(total)

    for x in make():
        print("value", x)

    for c in Countdown(3):
        print("countdown", c)
    return 0

// expect:
// 120
// value 0.5
// value 1.5
// countdown 3
// countdown 2
// countdown 1
