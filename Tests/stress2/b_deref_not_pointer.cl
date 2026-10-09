// N12: *n with n not a pointer is a clear error (it used to crash the compiler inside print)
function main() -> int32:
    let n = 5
    print(*n)
    return 0

// expect-error: Only a pointer can be dereferenced
