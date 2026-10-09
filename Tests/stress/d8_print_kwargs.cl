// stress test D8_print_kwargs
// expect-error: E069


function main() -> int32:
    print("a", bogus = 3)
    return 0
