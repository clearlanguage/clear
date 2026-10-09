// stress test M16_untyped_lambda_map
// expect:
// 6


function main() -> int32:
    let ops = Map[str, function(int) -> int]()
    ops["inc"] = lambda x: x + 1
    print(ops["inc"](5))
    return 0
