function main() -> int32:
    let l = List[int]()
    l.push(1)
    let r = l.remove(0)
    return 0

// expect-error: ‘remove’ doesn't return a value
