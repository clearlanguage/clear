// stress test D1_init_count
// expect-error: E041


class P:
    x: int
    function init(self, x: int):
        self.x = x
function main() -> int32:
    let p = P()
    return 0
