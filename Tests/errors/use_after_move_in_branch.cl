class Handle:                       // cleans up itself and has no operator copy: it moves
    name: str

    operator destruct(self):
        print("close", self.name)

function take(h: Handle):
    print(h.name)

function main(argc: int32) -> int32:
    let a = Handle("file")
    if argc > 0:
        take(a)            // moved on one path
    print(a.name)          // ...so it may be empty here
    return 0

// expect-error: This variable was moved
