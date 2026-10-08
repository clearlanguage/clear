// any C function can be declared and called; the C library is linked in
declare printf(format: *int8, args: ...) -> int32
declare abs(n: int32) -> int32
declare getenv(name: *int8) -> *int8

function main() -> int32:
    printf("%d + %d = %d\n", 2, 3, 2 + 3)
    printf("%.2f\n", 3.14159)
    print(abs(-7), getenv("CLEAR_SURELY_NOT_SET") == null)
    return 0

// expect:
// 2 + 3 = 5
// 3.14
// 7 true
