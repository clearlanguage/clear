enum Token:
    Num(value: int)
    Plus
    Times
    End

function eval(tokens: *[6; Token]) -> int:
    let total = 0
    let term = 1
    for t in *tokens:
        switch t:
            case Num(v):
                term *= v
            case Plus:
                total += term
                term = 1
            case Times:
                pass
            case End:
                total += term
    return total

function main() -> int32:
    // 2 * 3 + 4
    let toks: [6; Token] = {Token.Num(2), Token.Times, Token.Num(3), Token.Plus, Token.Num(4), Token.End}
    print(eval(&toks))
    return 0

// expect:
// 10
