class Account:
    balance: float64

    function __init__(self: *Account, deposit: float64):
        self.balance = deposit

function main() -> int32:
    let a = Account()
    return 0

// expect-error
