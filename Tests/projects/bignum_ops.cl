// Regression test (numeric stress round 3): arbitrary-precision integers written naturally:
// `self + self`, `self * self`, `operator negate`, `x += x` / `x *= x` on an owning class,
// `a, b = b, a + b`, `x == x` on a last use, and `f250 * (num(2) * f251 - f250)`.
// Expected output computed with Python.
// Arbitrary precision unsigned/signed integer, base 1e9 limbs stored little-endian.

const BASE: int64 = 1000000000

class BigInt:
    limbs: List[int64]
    negative: bool

    function init(self, value: int64):
        self.limbs = List[int64]()
        self.negative = value < 0
        let v: uint64 = when value < 0 use (-(value + 1)) as uint64 + 1 otherwise value as uint64
        if v == 0:
            self.limbs.push(0)
        while v > 0:
            self.limbs.push((v % 1000000000) as int64)
            v = v / 1000000000

    function trim(self):
        while len(self.limbs) > 1 and self.limbs.last() == 0:
            self.limbs.pop()
        if len(self.limbs) == 1 and self.limbs[0] == 0:
            self.negative = false

    function is_zero(self) -> bool:
        return len(self.limbs) == 1 and self.limbs[0] == 0

// magnitude comparison: -1, 0, 1
function compare_abs(a: *BigInt, b: *BigInt) -> int:
    if len(a.limbs) != len(b.limbs):
        return when len(a.limbs) < len(b.limbs) use -1 otherwise 1
    let i = len(a.limbs) - 1
    while i >= 0:
        if a.limbs[i] != b.limbs[i]:
            return when a.limbs[i] < b.limbs[i] use -1 otherwise 1
        i -= 1
    return 0

function add_abs(a: *BigInt, b: *BigInt) -> BigInt:
    let r = BigInt(0)
    r.limbs.clear()
    let n = when len(a.limbs) > len(b.limbs) use len(a.limbs) otherwise len(b.limbs)
    let carry: int64 = 0
    for i in 0..n:
        let s = carry
        if i < len(a.limbs):
            s += a.limbs[i]
        if i < len(b.limbs):
            s += b.limbs[i]
        r.limbs.push(s % BASE)
        carry = s / BASE
    if carry > 0:
        r.limbs.push(carry)
    return r

// requires |a| >= |b|
function sub_abs(a: *BigInt, b: *BigInt) -> BigInt:
    let r = BigInt(0)
    r.limbs.clear()
    let borrow: int64 = 0
    for i in 0..len(a.limbs):
        let d = a.limbs[i] - borrow
        if i < len(b.limbs):
            d -= b.limbs[i]
        if d < 0:
            d += BASE
            borrow = 1
        else:
            borrow = 0
        r.limbs.push(d)
    r.trim()
    return r

class Num:
    value: BigInt

    operator add(self, other: Num) -> Num:
        let r = Num(BigInt(0))
        if self.value.negative == other.value.negative:
            r.value = add_abs(&self.value, &other.value)
            r.value.negative = self.value.negative
        else:
            let c = compare_abs(&self.value, &other.value)
            if c >= 0:
                r.value = sub_abs(&self.value, &other.value)
                r.value.negative = self.value.negative
            else:
                r.value = sub_abs(&other.value, &self.value)
                r.value.negative = other.value.negative
        r.value.trim()
        return r

    operator subtract(self, other: Num) -> Num:
        return self + -other

    operator negate(self) -> Num:
        let a = *self
        a.value.negative = not a.value.negative
        if a.value.is_zero():
            a.value.negative = false
        return a

    function doubled(self) -> Num:
        return self + self

    function squared(self) -> Num:
        return self * self

    operator multiply(self, other: Num) -> Num:
        let na = len(self.value.limbs)
        let nb = len(other.value.limbs)
        let acc = List[uint64]()
        for i in 0..(na + nb):
            acc.push(0)
        for i in 0..na:
            let carry: uint64 = 0
            let ai = self.value.limbs[i] as uint64
            for j in 0..nb:
                let cur = acc[i + j] + ai * (other.value.limbs[j] as uint64) + carry
                acc[i + j] = cur % 1000000000
                carry = cur / 1000000000
            let k = i + nb
            while carry > 0:
                let cur = acc[k] + carry
                acc[k] = cur % 1000000000
                carry = cur / 1000000000
                k += 1
        let r = Num(BigInt(0))
        r.value.limbs.clear()
        for x in acc:
            r.value.limbs.push(x as int64)
        r.value.negative = self.value.negative != other.value.negative
        r.value.trim()
        return r

    operator equals(self, other: Num) -> bool:
        return self.value.negative == other.value.negative and compare_abs(&self.value, &other.value) == 0

    operator less(self, other: Num) -> bool:
        if self.value.negative != other.value.negative:
            return self.value.negative
        let c = compare_abs(&self.value, &other.value)
        return when self.value.negative use c > 0 otherwise c < 0

    function to_string(self) -> String:
        let s = String("")
        if self.value.negative:
            s.append("-")
        let top = len(self.value.limbs) - 1
        s.append_int(self.value.limbs[top])
        let i = top - 1
        while i >= 0:
            let part = from_int(self.value.limbs[i])
            for pad in 0..(9 - len(part)):
                s.append("0")
            s.append(part)
            i -= 1
        return s

function num(v: int64) -> Num:
    return Num(BigInt(v))

function factorial(n: int) -> Num:
    let r = num(1)
    for i in 2..=n:
        r *= num(i)
    return r

function power(b: Num, e: int) -> Num:
    let result = num(1)
    let base = b
    let k = e
    while k > 0:
        if k & 1 == 1:
            result *= base
        base *= base
        k >>= 1
    return result

function fib(n: int) -> Num:
    let a = num(0)
    let b = num(1)
    for i in 0..n:
        a, b = b, a + b
    return a

function main() -> int32:
    print(factorial(100).to_string())
    print(power(num(2), 256).to_string())
    print(fib(500).to_string())
    let x = num(-123456789012)
    let y = num(987654321)
    print((x + y).to_string(), (x - y).to_string(), (y - x).to_string(), (x * y).to_string())
    print(x.doubled().to_string(), x.squared().to_string(), (x + x).to_string(), (-x).to_string(), (-(-x)).to_string(), (-num(0)).to_string())
    print(x < y, y < x, x == x, num(0) - num(0) == num(0), (num(5) - num(5)).to_string())
    let big = power(num(10), 30) - num(1)
    print(big.to_string(), (big * big).to_string())
    print(big == big, big < big, (big - big).to_string(), (big + big).to_string())
    print(num(9223372036854775807).to_string(), num(-9223372036854775808).to_string())
    // compare fib via identity F(2n) = F(n) * (2F(n+1) - F(n))
    let f250 = fib(250)
    let f251 = fib(251)
    let lhs = fib(500)
    let two_f251 = num(2) * f251
    let diff = two_f251 - f250
    print(two_f251.to_string())
    print(diff.to_string())
    let rhs = f250 * diff
    print(rhs.to_string())
    print(lhs == rhs, fib(500) == f250 * (num(2) * f251 - f250), lhs == lhs)
    let acc = num(7)
    acc += acc
    acc *= acc
    acc -= num(1)
    print(acc.to_string(), (acc - acc).to_string(), (-acc + acc).to_string())
    return 0

// expect:
// 93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000
// 115792089237316195423570985008687907853269984665640564039457584007913129639936
// 139423224561697880139724382870407283950070256587697307264108962948325571622863290691557658876222521294125
// -122469134691 -124444443333 124444443333 -121932631124487120852
// -246913578024 15241578753153483936144 -246913578024 123456789012 -123456789012 0
// true false true true 0
// 999999999999999999999999999999 999999999999999999999999999998000000000000000000000000000001
// true false 0 1999999999999999999999999999998
// 9223372036854775807 -9223372036854775808
// 25553047145849465172074067789310063797319112894704498
// 17656721319717734662791328845675730903632844218828123
// 139423224561697880139724382870407283950070256587697307264108962948325571622863290691557658876222521294125
// true true true
// 195 0 0
