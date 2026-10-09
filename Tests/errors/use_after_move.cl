class Handle:                       // cleans up itself and has no operator copy: it moves
    name: str

    operator destruct(self):
        print("close", self.name)

function main() -> int32:
    let a = Handle("file")
    let b = a              // a can't be copied, so it moves into b and is left empty
    print(a.name)          // reading it is an error, not an empty value
    return 0

// expect-error: This variable was moved
