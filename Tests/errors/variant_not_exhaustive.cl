enum Token:
    Number(value: int)
    Word(text: str)
    End

function main() -> int32:
    let t = Token.Number(1)
    switch t:
        case Number(n):
            print(n)
        case End:
            print("end")
    return 0

// expect-error
