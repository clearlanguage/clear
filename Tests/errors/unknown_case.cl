enum Token:
    Number(value: int)
    End

function main() -> int32:
    let t = Token.Number(1)
    print(t is Token.Word)
    return 0

// expect-error
