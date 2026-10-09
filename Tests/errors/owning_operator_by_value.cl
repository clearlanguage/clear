import "string"

class Tag:
    text: String

    operator equals(self, other: Tag) -> bool:
        return true

function main() -> int32:
    let a = Tag(String("a"))
    let b = Tag(String("b"))
    print(a == b)
    return 0

// expect-error
