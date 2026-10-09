class Handle:                       // cleans up itself and has no operator copy: it moves
    name: str

    operator destruct(self):
        print("close", self.name)

function take(h: Handle):
    print(h.name)

function main() -> int32:
    let h = Handle("file")
    for i in 0..3:
        take(h)            // the second time round, h is already empty
    return 0

// expect-error: the next time round the loop uses again
