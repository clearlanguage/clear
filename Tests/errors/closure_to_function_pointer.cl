function apply(f: function(int) -> int, x: int) -> int:
    return f(x)

function main() -> int32:
    let k = 2
    print(apply(lambda x: x * k, 3))
    return 0

// expect-error
