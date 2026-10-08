function one() -> int:
    return 1

async function run() -> int:
    return await one()

function main() -> int32:
    return 0

// expect-error
