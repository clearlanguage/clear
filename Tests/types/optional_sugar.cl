// if r: / if not r: return / ?? / := / ?. : optionals without .value
import "string"
import "list"
class Address:
    city: String
class User:
    name: String
    address: ?Address
    function greet(self):
        print("hi, I'm", self.name)
    function nick(self) -> ?String:
        return when len(self.name) > 3 use String("big") otherwise none
function find(xs: *List[int], target: int) -> ?int:
    for i in 0..len(*xs):
        if xs[i] == target:
            return i as int
    return none
function first_even(xs: *List[int]) -> int:
    let r = find(xs, 4)
    if not r:
        return -1
    return r * 100
function main() -> int32:
    let xs = List[int]()
    for i in 1..=5:
        xs.push(i)
    let r = find(&xs, 3)
    if r:
        print("found at", r, r + 1)
    else:
        print("missing")
    let m = find(&xs, 9)
    if not m:
        print("9 is missing")
    else:
        print("9 at", m)
    print(first_even(&xs))
    print(find(&xs, 2) ?? -1, find(&xs, 42) ?? -1)
    let a: ?int = none
    let b: ?int = 7
    print((a ?? b) ?? 0, a ?? b ?? 0)
    if idx := find(&xs, 5):
        print("walrus found", idx)
    else if other := find(&xs, 1):
        print("elif walrus", other)
    let u: ?User = User(String("ada"), Address(String("london")))
    let nobody: ?User = none
    print(u?.name ?? String("?"), nobody?.name ?? String("nobody"))
    let city = u?.address?.city
    print(city ?? String("no city"), nobody?.address?.city ?? String("no city"))
    u?.greet()
    nobody?.greet()
    print(u?.nick() ?? String("none"))
    let count = 0
    let lines = List[String]()
    lines.push(String("one"))
    lines.push(String("two"))
    let i = 0
    while line := (when i < len(lines) use lines[i] otherwise none):
        print("line", line)
        i += 1
    let s: ?String = String("text")
    if s:
        s.append("!")
        print(s, len(s))
    print(s ?? String("x"))
    return 0

// expect:
// found at 2 3
// 9 is missing
// 300
// 1 -1
// 7 7
// walrus found 4
// ada nobody
// london no city
// hi, I'm ada
// none
// line one
// line two
// text! 5
// text!
