class A(B):
    x: int

class B(A):
    y: int

function main() -> int32:
    return 0

// expect-error
