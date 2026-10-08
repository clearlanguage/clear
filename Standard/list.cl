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
        self.data[self.length] = value
        self.length += 1

    function pop(self: *List[T]) -> T:
        self.length -= 1
        return self.data[self.length]

    function __getitem__(self: *List[T], index: int64) -> T:
        return self.data[index]

    function __setitem__(self: *List[T], index: int64, value: T):
        self.data[index] = value

    function __len__(self: *List[T]) -> int64:
        return self.length

    function last(self: *List[T]) -> T:
        return self.data[self.length - 1]

    function is_empty(self: *List[T]) -> bool:
        return self.length == 0

    function clear(self: *List[T]):
        self.length = 0

    function contains(self: *List[T], value: T) -> bool:
        for i in 0..self.length:
            if self.data[i] == value:
                return true
        return false

    function free(self: *List[T]):
        if self.data != null:
            release(self.data)
        self.data = null
        self.length = 0
        self.capacity = 0
