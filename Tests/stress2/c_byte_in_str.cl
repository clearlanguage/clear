// a byte in a str: is it one of those bytes (D3)
function main() -> int32:
    let c: int8 = 43
    let d = 'x'
    let u: uint8 = 45
    print(c in "+-", d in "+-", u in "+-", d not in "+-", 
'-' in "+-")
    return 0

// expect:
// true false true true true
