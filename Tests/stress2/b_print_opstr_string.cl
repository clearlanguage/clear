// N23: operator str returning a String is used inside containers too (and the String is cleaned up)
class B:
    x: int

    operator str(self) -> String:
        return "B" + from_int(self.x as int64)

class Box[T]:
    value: T

    operator str(self) -> String:
        return "<" + from_int(self.value as int64) + ">"

class A:
    x: int

    operator str(self) -> str:
        return "A!"

function main() -> int32:
    let lb = List[B]()
    lb.push(B(1))
    lb.push(B(2))
    print(lb[0], lb, lb[:])

    let m = Map[str, B]()
    m["k"] = B(3)
    print(m)

    let o: ?B = B(4)
    let none_b: ?B = none
    print(o, none_b, (B(5), A(0)), [B(6), B(7)])

    let boxes = List[Box[int]]()
    boxes.push(Box(8))
    print(boxes)
    return 0

// expect:
// B1 [B1, B2] [B1, B2]
// {k: B3}
// B4 none (B5, A!) [B6, B7]
// [<8>]
