// N1: the last use of a variable must not take (move) it while another use in the same statement still reads it

function size(s: String) -> int64:
    return len(s)

class Bag:
    n: int

    function show(self, s: String):
        print("show got [", s, "]")

function pick(b: *Bag, s: String) -> *Bag:
    print("pick got [", s, "]")
    return b

class N:
    items: List[int]
    operator multiply(self, other: N) -> int64:
        return len(self.items) * 10 + len(other.items)

function grow(n: N) -> N:
    n.items.push(9)
    return n

class Pos:
    n: int

class V:
    s: String
    operator add(self, other: V) -> V:
        return V(self.s + other.s)

class Ord:
    name: String

function bump(m: *Map[String, Pos], o: *Ord):
    m[o.name].n += 1

function main() -> int32:
    let k = String("gus")
    print(k, size(k))

    let m = Map[String, int]()
    m["k"] = 5
    let key = String("k")
    m[key] += 1
    print(m)

    let bag = Bag(0)
    let v = String("hello")
    pick(&bag, v).show(v)

    let a = N(List[int]())
    a.items.push(1)
    print(a * grow(a))

    let places = Map[String, Pos]()
    places[String("gus")] = Pos()
    let name = String("gus")
    places[name].n = 7
    let o = Ord(String("gus"))
    bump(&places, &o)
    print(places)

    let w = String("w")
    let pair = (w, w)
    print(pair[0], pair[1])

    let twice = V(String("ab"))
    twice += twice
    print(twice.s)
    return 0

// expect:
// gus 3
// {k: 6}
// pick got [ hello ]
// show got [ hello ]
// 12
// {gus: Pos(n=8)}
// w w
// abab
