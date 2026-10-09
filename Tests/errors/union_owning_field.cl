import "string"

union Slot:
    number: int
    text: String           // a union cannot know whether to clean this up

function main() -> int32:
    return 0

// expect-error: A union cannot hold a value that needs cleaning up
