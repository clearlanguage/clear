// Regression test (numeric stress round 3): Q16.16 fixed point, generic Complex[T] with
// operator negate, generic compound assignment (+=, -=, *=) on class and number T,
// polynomial multiplication (naive, Karatsuba, FFT), Newton and Durand-Kerner roots.
// Expected output computed with Python/numpy.
// Fixed-point, Complex[T], polynomials (naive, Karatsuba, FFT), roots (Newton, Durand-Kerner).
import "math"

// ---------- fixed point Q16.16 ----------
class Fixed:
    raw: int32

    operator add(self, o: Fixed) -> Fixed:
        return Fixed(self.raw + o.raw)
    operator subtract(self, o: Fixed) -> Fixed:
        return Fixed(self.raw - o.raw)
    operator multiply(self, o: Fixed) -> Fixed:
        return Fixed(((self.raw as int64 * o.raw as int64) >> 16) as int32)
    operator divide(self, o: Fixed) -> Fixed:
        return Fixed(((self.raw as int64 << 16) / o.raw as int64) as int32)
    operator less(self, o: Fixed) -> bool:
        return self.raw < o.raw
    operator negate(self) -> Fixed:
        return Fixed(-self.raw)
    function to_float(self) -> float64:
        return self.raw as float64 / 65536.0

function fx(x: float64) -> Fixed:
    return Fixed((x * 65536.0) as int32)

function fixed_sqrt(x: Fixed) -> Fixed:
    let g = x / fx(2.0) + fx(0.5)
    for i in 0..20:
        g = (g + x / g) / fx(2.0)
    return g

// ---------- Complex[T] ----------
class Complex[T]:
    re: T
    im: T

    operator add(self, o: Complex[T]) -> Complex[T]:
        return Complex[T](self.re + o.re, self.im + o.im)
    operator subtract(self, o: Complex[T]) -> Complex[T]:
        return Complex[T](self.re - o.re, self.im - o.im)
    operator multiply(self, o: Complex[T]) -> Complex[T]:
        return Complex[T](self.re * o.re - self.im * o.im, self.re * o.im + self.im * o.re)
    operator divide(self, o: Complex[T]) -> Complex[T]:
        let d = o.norm2()
        let n = *self * o.conj()
        return Complex[T](n.re / d, n.im / d)
    operator equals(self, o: Complex[T]) -> bool:
        return self.re == o.re and self.im == o.im
    operator negate(self) -> Complex[T]:
        return Complex[T](-self.re, -self.im)
    function conj(self) -> Complex[T]:
        return Complex[T](self.re, -self.im)
    function norm2(self) -> T:
        return self.re * self.re + self.im * self.im
    function scale(self, k: T) -> Complex[T]:
        return Complex[T](self.re * k, self.im * k)

function cpow[T](base: Complex[T], e: int64) -> Complex[T]:
    let r = Complex[T](T(1), T(0))
    let b = base
    let k = e
    while k > 0:
        if k & 1 == 1:
            r *= b
        b *= b
        k >>= 1
    return r

function cabs(z: Complex[float64]) -> float64:
    return sqrt(z.norm2())

// ---------- polynomials (coefficients low -> high) ----------
function poly_mul_naive[T](a: []T, b: []T) -> List[T]:
    let r = List[T]()
    for i in 0..(len(a) + len(b) - 1):
        r.push(T(0))
    for i in 0..len(a):
        for j in 0..len(b):
            r[i + j] += a[i] * b[j]
    return r

// both inputs have the same length n
function karatsuba[T](a: []T, b: []T) -> List[T]:
    let n = len(a)
    if n <= 16:
        return poly_mul_naive(a, b)
    let h = n / 2
    let a0 = a[:h]
    let a1 = a[h:]
    let b0 = b[:h]
    let b1 = b[h:]
    // pad the halves to the same length
    let sa = List[T]()
    let sb = List[T]()
    for i in 0..len(a1):
        sa.push(a1[i] + (when i < h use a0[i] otherwise T(0)))
        sb.push(b1[i] + (when i < h use b0[i] otherwise T(0)))
    let lo = karatsuba(a0, b0)
    let hi = karatsuba(a1, b1)
    let mid = karatsuba(sa[:], sb[:])
    let r = List[T]()
    for i in 0..(2 * n - 1):
        r.push(T(0))
    for i in 0..len(lo):
        r[i] += lo[i]
        mid[i] -= lo[i]
    for i in 0..len(hi):
        r[i + 2 * h] += hi[i]
        mid[i] -= hi[i]
    for i in 0..len(mid):
        if i + h < len(r):
            r[i + h] += mid[i]
    return r

function horner[T](p: []T, x: T) -> T:
    let acc = T(0)
    let i = len(p) - 1
    while i >= 0:
        acc = acc * x + p[i]
        i -= 1
    return acc

function derivative(p: []float64) -> List[float64]:
    let d = List[float64]()
    for i in 1..len(p):
        d.push(p[i] * i as float64)
    return d

function newton(p: []float64, x0: float64) -> (float64, int):
    let dp = derivative(p)
    let x = x0
    let steps = 0
    while steps < 100:
        let fx0 = horner(p, x)
        let step = fx0 / horner(dp[:], x)
        x -= step
        steps += 1
        if abs(step) < 1e-15:
            break
    return (x, steps)

// monic polynomial with complex coefficients (low -> high), roots by Durand-Kerner
function durand_kerner(p: []Complex[float64]) -> List[Complex[float64]]:
    let n = len(p) - 1
    let roots = List[Complex[float64]]()
    let seed = Complex[float64](0.4, 0.9)
    for i in 0..n:
        roots.push(cpow(seed, i))
    for iter in 0..500:
        let delta = 0.0
        for i in 0..n:
            let num = horner(p, roots[i])
            let den = Complex[float64](1.0, 0.0)
            for j in 0..n:
                if j != i:
                    den *= roots[i] - roots[j]
            let step = num / den
            roots[i] -= step
            delta = max(delta, cabs(step))
        if delta < 1e-14:
            break
    return roots

// ---------- FFT (iterative radix-2) ----------
function fft(a: []Complex[float64], invert: bool):
    let n = len(a)
    let j: int64 = 0
    for i in 1..n:
        let bit = n >> 1
        while j & bit != 0:
            j ^= bit
            bit >>= 1
        j ^= bit
        if i < j:
            a[i], a[j] = a[j], a[i]
    let length: int64 = 2
    while length <= n:
        let ang = TAU / length as float64 * (when invert use -1.0 otherwise 1.0)
        let wl = Complex[float64](cos(ang), sin(ang))
        let i: int64 = 0
        while i < n:
            let w = Complex[float64](1.0, 0.0)
            for k in 0..(length / 2):
                let u = a[i + k]
                let v = a[i + k + length / 2] * w
                a[i + k] = u + v
                a[i + k + length / 2] = u - v
                w *= wl
            i += length
        length <<= 1
    if invert:
        for i in 0..n:
            a[i] = a[i].scale(1.0 / n as float64)

function fft_mul(a: []int64, b: []int64) -> List[int64]:
    let n: int64 = 1
    while n < len(a) + len(b):
        n <<= 1
    let fa = List[Complex[float64]]()
    let fb = List[Complex[float64]]()
    for i in 0..n:
        fa.push(Complex[float64](when i < len(a) use a[i] as float64 otherwise 0.0, 0.0))
        fb.push(Complex[float64](when i < len(b) use b[i] as float64 otherwise 0.0, 0.0))
    fft(fa[:], false)
    fft(fb[:], false)
    for i in 0..n:
        fa[i] *= fb[i]
    fft(fa[:], true)
    let r = List[int64]()
    for i in 0..(len(a) + len(b) - 1):
        r.push(round(fa[i].re) as int64)
    return r

function r6(x: float64) -> float64:
    let v = round(x * 1e6) / 1e6
    return when v == 0.0 use 0.0 otherwise v

function main() -> int32:
    // fixed point
    let a = fx(3.25)
    let b = fx(-1.5)
    print("fixed", (a + b).to_float(), (a - b).to_float(), (a * b).to_float(), (a / b).to_float(), b < a)
    print("fsqrt", fixed_sqrt(fx(2.0)).to_float(), fixed_sqrt(fx(10.0)).raw, (-a).to_float(), (-b + a).to_float())

    // gaussian integers and complex floats
    let g = Complex[int64](3, 4)
    let h = Complex[int64](1, -2)
    let gh = g * h
    let g10 = cpow(g, 10)
    print("gauss", gh.re, gh.im, g10.re, g10.im, g.norm2(), g == g, g == h)
    let ng = -g
    let gg = g
    gg += gg
    gg *= h
    print("neg", ng.re, ng.im, (-(-g) == g), gg.re, gg.im, (g * g).re, (g + g - g == g))
    let z = Complex[float64](1.5, -0.5)
    let q = z / Complex[float64](0.25, 2.0)
    print("cdiv", r6(q.re), r6(q.im), r6(cabs(cpow(z, 7))))
    let zf = Complex[float32](1.5, 2.0)
    let zf2 = zf * zf
    print("c32", zf2.re, zf2.im, zf.norm2())

    // polynomials
    let seed: uint64 = 12345
    let pa = List[int64]()
    let pb = List[int64]()
    for i in 0..300:
        seed = seed * 6364136223846793005 + 1442695040888963407
        pa.push((seed >> 40) as int64 % 1000)
        seed = seed * 6364136223846793005 + 1442695040888963407
        pb.push((seed >> 40) as int64 % 1000 - 500)
    let n1 = poly_mul_naive(pa[:], pb[:])
    let k1 = karatsuba(pa[:], pb[:])
    let f1 = fft_mul(pa[:], pb[:])
    let check: int64 = 0
    let same = len(n1) == len(k1) and len(k1) == len(f1)
    for i in 0..len(n1):
        if n1[i] != k1[i] or n1[i] != f1[i]:
            same = false
        check = (check * 31 + n1[i]) % 1000000007
    print("polymul", len(n1), same, check, n1[0], n1[299], n1[598])

    // Newton on x^3 - 2x - 5 and sqrt(2)
    let wallis = [-5.0, -2.0, 0.0, 1.0]
    let root, steps = newton(wallis[:], 2.0)
    let s2 = [-2.0, 0.0, 1.0]
    let r2, steps2 = newton(s2[:], 1.0)
    print("newton", root, r2, r2 == sqrt(2.0), steps > 0 and steps2 > 0)

    // Durand-Kerner on (x-1)(x-2)(x-3)(x+1)(x^2+1)
    let poly = List[Complex[float64]]()
    poly.push(Complex[float64](1.0, 0.0))
    let rs = [1.0, 2.0, 3.0, -1.0]
    for r in rs:
        let lin = [Complex[float64](-r, 0.0), Complex[float64](1.0, 0.0)]
        poly = poly_mul_naive(poly[:], lin[:])
    let quad = [Complex[float64](1.0, 0.0), Complex[float64](0.0, 0.0), Complex[float64](1.0, 0.0)]
    poly = poly_mul_naive(poly[:], quad[:])
    for c in poly:
        print("coef", c.re, c.im)
    let roots = durand_kerner(poly[:])
    // insertion sort by (re, im)
    for i in 1..len(roots):
        let j = i
        while j > 0 and (roots[j].re < roots[j - 1].re - 1e-9 or (abs(roots[j].re - roots[j - 1].re) < 1e-9 and roots[j].im < roots[j - 1].im)):
            roots[j], roots[j - 1] = roots[j - 1], roots[j]
            j -= 1
    for r in roots:
        print("root", r6(r.re), r6(r.im))

    // FFT of a small signal
    let sig = List[Complex[float64]]()
    for i in 0..8:
        sig.push(Complex[float64](when i < 4 use (i + 1) as float64 otherwise 0.0, 0.0))
    fft(sig[:], false)
    for c in sig:
        print("fft", r6(c.re), r6(c.im))
    return 0

// expect:
// fixed 1.75 4.75 -4.875 -2.166656494140625 true
// fsqrt 1.4141998291015625 207243 -3.25 4.75
// gauss 11 -2 -9653287 1476984 25 true false
// neg -3 -4 true 22 -4 -7 true
// cdiv -0.153846 -0.769231 24.705294
// c32 -1.75 6.0 6.25
// polymul 599 true -148673245 -31302 -462548 -25286
// newton 2.0945514815423265 1.414213562373095 false true
// coef -6.0 0.0
// coef 5.0 0.0
// coef -1.0 0.0
// coef 0.0 0.0
// coef 6.0 0.0
// coef -5.0 0.0
// coef 1.0 0.0
// root -1.0 0.0
// root 0.0 -1.0
// root 0.0 1.0
// root 1.0 0.0
// root 2.0 0.0
// root 3.0 0.0
// fft 10.0 0.0
// fft -0.414214 7.242641
// fft -2.0 -2.0
// fft 2.414214 1.242641
// fft -2.0 0.0
// fft 2.414214 -1.242641
// fft -2.0 2.0
// fft -0.414214 -7.242641
