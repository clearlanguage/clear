enum Token:
    Number(value: int)
    End

function main() -> int32:
    let t = Token.Number
    return 0

// expect-error
