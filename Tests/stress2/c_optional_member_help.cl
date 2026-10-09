class Box:
    items: List[int]

function find(b: *Box, ok: bool) -> ?*Box:
    if ok:
        return b
    return none

function main() -> int32:
    let b = Box { }
    b.items.push(4)
    print(find(&b, true)?.items.contains(4))
    return 0

// expect-error: ‘contains’ belongs to ‘List[int32]’, but this is a ‘?List[int32]’. Use ?.
