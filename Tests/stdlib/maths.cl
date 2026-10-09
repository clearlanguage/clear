import "math"

function main() -> int32:
    print(sqrt(16.0), floor(2.7), ceil(2.1), pow(2.0, 10.0))
    print(min(3, 9), max(2.5, 1.5), clamp(15, 0, 10), clamp(-3, 0, 10))
    print(abs(-7), abs(2.5), sign(-4), sign(0))
    print(gcd(48, 18), is_prime(97), is_prime(91))
    print(round(degrees(PI)), round(radians(180.0) * 1000.0))
    return 0

// expect:
// 4.0 2.0 3.0 1024.0
// 3 2.5 10 0
// 7 2.5 -1 0
// 6 true false
// 180.0 3142.0
