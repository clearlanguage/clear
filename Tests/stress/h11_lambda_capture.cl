// stress test H11_lambda_capture
// expect:
// 8


function main() -> int32:
    let n = 5
    let s = String("abc")
    let f = lambda (x: int): x + n + len(s)
    n = 100
    s.append("defgh")
    print(f(0))
    return 0
