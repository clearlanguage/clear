import "string"

function main() -> int32:
    let name = String("ada")
    let make = lambda: lambda: print(name)   // the inner lambda borrows name and is returned
    return 0

// expect-error: This lambda borrows local variables
