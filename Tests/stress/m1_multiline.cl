// stress test M1_multiline
// expect:
// ab 3 3


function main() -> int32:
    let log = "a" +
              "b"
    let x = 1 +
        2
    let y = (1 +
        2)
    print(log, x, y)
    return 0
