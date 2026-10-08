import "list"

class Stack[T]:
    items: List[T]

    function push(self: *Stack[T], v: T):
        self.items.push(v)

    function pop(self: *Stack[T]) -> ?T:
        if self.items.is_empty():
            return none
        return self.items.pop()

    function size(self: *Stack[T]) -> int64:
        return len(self.items)

function balanced(text: str) -> bool:
    let s = Stack[int8](List[int8]())
    defer s.items.free()
    let i = 0
    while text[i] != 0:
        let c = text[i]
        if c == '(' or c == '[':
            s.push(c)
        else if c == ')' or c == ']':
            let top = s.pop()
            if top is none:
                return false
            if (c == ')' and top.value != '(') or (c == ']' and top.value != '['):
                return false
        i += 1
    return s.size() == 0

function main() -> int32:
    print(balanced("([]())"), balanced("(]"), balanced("(("), balanced(""))
    return 0

// expect:
// true false false true
