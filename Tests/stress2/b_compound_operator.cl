// N6: a op= b on a class uses its operator op (a = a op b), also on list elements and in generic code
class V:
    x: int

    operator add(self, o: V) -> V:
        return V(self.x + o.x)

    operator subtract(self, o: V) -> V:
        return V(self.x - o.x)

    operator multiply(self, o: V) -> V:
        return V(self.x * o.x)

    operator divide(self, k: int) -> V:
        return V(self.x / k)

    operator str(self) -> String:
        return from_int(self.x as int64)

class Text:
    s: String

    operator add(self, o: str) -> Text:
        return Text(self.s + o)

function convolve[T](a: List[T], b: List[T], zero: T) -> List[T]:
    let r = List[T]()
    for i in 0..len(a) + len(b) - 1:
        r.push(zero)
    for i in 0..len(a):
        for j in 0..len(b):
            r[i + j] += a[i] * b[j]
    return r

// the same statement emitted at every exit of the function
function bump(n: int, total: *V) -> int:
    defer *total += V(1)
    if n > 0:
        return 1
    return 2

function main() -> int32:
    let a = V(1)
    a += V(2)
    a *= V(5)
    a -= V(3)
    a /= 2
    print(a.x)

    let xs = List[V]()
    xs.push(V(1))
    xs[0] += V(5)
    xs[0] *= V(2)
    print(xs[0].x)

    let p = &a
    *p += V(10)
    print(a.x)

    let t = Text(String("ab"))
    t += "cd"
    t += "!"
    print(t.s)

    let ps = List[V]()
    ps.push(V(1))
    ps.push(V(2))
    let qs = List[V]()
    qs.push(V(3))
    qs.push(V(4))
    print(convolve(ps, qs, V(0)))

    let count = V(0)
    bump(1, &count)
    bump(0, &count)
    print(count.x)
    return 0

// expect:
// 6
// 12
// 16
// abcd!
// [3, 10, 8]
// 2
