// elif is another spelling of else if (as is elseif)
function size(x: int) -> str:
    if x > 5:
        return "big"
    elif x > 2:
        return "medium"
    elseif x > 0:
        return "small"
    else if x == 0:
        return "zero"
    else:
        return "negative"

function main() -> int32:
    print(size(9), size(3), size(1), size(0), size(-1))
    return 0

// expect:
// big medium small zero negative
