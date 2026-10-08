// list: a growable array, like Python's list but holding one type.
//
//     import "list"
//     let numbers = List[int]()
//     defer numbers.free()
//     numbers.push(4)
//     print(numbers[0], numbers.length)

import "memory"

class List[T]:
    data: *T
    length: int64
    capacity: int64

    function push(self: *List[T], value: T):
        if self.length == self.capacity:
            let grown = when self.capacity == 0 use 8 otherwise self.capacity * 2
            self.data = reallocate(self.data, grown)
            self.capacity = grown
        *(self.data + self.length) = value
        self.length += 1

    function pop(self: *List[T]) -> T:
        assert self.length > 0, "pop from an empty List"
        self.length -= 1
        return *(self.data + self.length)

    // indices are checked like array indices (the checks disappear with --no-checks / -O3)
    operator get(self: *List[T], index: int64) -> T:
        assert index >= 0 and index < self.length, "List index out of range"
        return *(self.data + index)

    operator set(self: *List[T], index: int64, value: T):
        assert index >= 0 and index < self.length, "List index out of range"
        *(self.data + index) = value

    operator len(self: *List[T]) -> int64:
        return self.length

    function last(self: *List[T]) -> T:
        assert self.length > 0, "last of an empty List"
        return *(self.data + self.length - 1)

    function is_empty(self: *List[T]) -> bool:
        return self.length == 0

    function clear(self: *List[T]):
        self.length = 0

    function contains(self: *List[T], value: T) -> bool:
        for i in 0..self.length:
            if *(self.data + i) == value:
                return true
        return false

    function free(self: *List[T]):
        if self.data != null:
            release(self.data)
        self.data = null
        self.length = 0
        self.capacity = 0
