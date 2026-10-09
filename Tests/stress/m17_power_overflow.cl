// stress test M17_power_overflow
// flags: --checks
// expect-exit: -6


function main() -> int32:
    let a = 3
    print(a ** 40)
    return 0
