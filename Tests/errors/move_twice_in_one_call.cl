class Handle:                       // cleans up itself and has no operator copy: it moves
    name: str

    operator destruct(self):
        print("close", self.name)

function both(a: Handle, b: Handle):
    print(a.name, b.name)

function main() -> int32:
    let a = Handle("file")
    both(a, a)             // the second argument finds a already moved
    return 0

// expect-error: This variable was moved
