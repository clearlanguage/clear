// math: constants and functions for numbers.
//
//     import "math"
//     print(sqrt(2.0), max(3, 7), clamp(15, 0, 10))

const PI  = 3.14159265358979323846
const TAU = 6.28318530717958647692
const E   = 2.71828182845904523536

// the C math library does the heavy lifting; these calls compile to single instructions where the CPU has them
declare sqrt(x: float64) -> float64
declare cbrt(x: float64) -> float64
declare pow(base: float64, exponent: float64) -> float64
declare exp(x: float64) -> float64
declare log(x: float64) -> float64
declare log2(x: float64) -> float64
declare log10(x: float64) -> float64
declare sin(x: float64) -> float64
declare cos(x: float64) -> float64
declare tan(x: float64) -> float64
declare asin(x: float64) -> float64
declare acos(x: float64) -> float64
declare atan(x: float64) -> float64
declare atan2(y: float64, x: float64) -> float64
declare sinh(x: float64) -> float64
declare cosh(x: float64) -> float64
declare tanh(x: float64) -> float64
declare floor(x: float64) -> float64
declare ceil(x: float64) -> float64
declare round(x: float64) -> float64
declare trunc(x: float64) -> float64
declare fmod(x: float64, y: float64) -> float64
declare hypot(x: float64, y: float64) -> float64

function min[T](a: T, b: T) -> T:
    return when a < b use a otherwise b

function max[T](a: T, b: T) -> T:
    return when a > b use a otherwise b

function clamp[T](value: T, low: T, high: T) -> T:
    return min(max(value, low), high)

function abs[T](value: T) -> T:
    return when value < 0 use -value otherwise value

function sign[T](value: T) -> T:
    if value > 0:
        return 1
    if value < 0:
        return -1
    return 0

function radians(degrees: float64) -> float64:
    return degrees * (PI / 180.0)

function degrees(radians: float64) -> float64:
    return radians * (180.0 / PI)

function logb(base: float64, x: float64) -> float64:
    return log(x) / log(base)

function lerp(a: float64, b: float64, t: float64) -> float64:
    return a + (b - a) * t

function gcd(a: int64, b: int64) -> int64:
    while b != 0:
        let rest = a % b
        a = b
        b = rest
    return abs(a)

function is_prime(n: int64) -> bool:
    if n < 2:
        return false
    let i: int64 = 2
    while i * i <= n:
        if n % i == 0:
            return false
        i += 1
    return true
