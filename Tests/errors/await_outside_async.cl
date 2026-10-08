async function one() -> int:
    return 1

function main() -> int32:
    let x = await one()
    return 0

// expect-error
