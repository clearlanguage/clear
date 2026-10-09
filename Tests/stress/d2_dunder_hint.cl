// stress test D2_dunder_hint
// expect-error: E056


function main() -> int32:
    let a: ?int = 1
    print(a + 1)
    return 0
