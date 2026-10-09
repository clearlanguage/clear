class Handle:                     // cleans up itself and has no operator copy: a lambda can only borrow it
    name: str

    operator destruct(self):
        self.name = ""

function main() -> int32:
    let h = Handle("file")
    let make = lambda: lambda: print(h.name)   // the inner lambda borrows h and is returned
    return 0

// expect-error: This lambda borrows local variables
