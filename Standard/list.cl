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
        return take(self.data + self.length)          // the list hands the item over

    // list[i] is the element itself: read it, assign to it (list[i] = v), or change it (list[i].count += 1).
    // Indices are checked like array indices (the checks disappear with --no-checks / -O3)
    operator get(self, index: int64) -> *T:
        assert index >= 0 and index < self.length, "List index out of range"
        return self.data + index

    operator len(self: *List[T]) -> int64:
        return self.length

    function last(self: *List[T]) -> T:
        assert self.length > 0, "last of an empty List"
        return self[self.length - 1]

    function is_empty(self: *List[T]) -> bool:
        return self.length == 0

    function clear(self: *List[T]):
        for i in 0..self.length:
            destroy(self.data + i)
        self.length = 0

    function contains(self: *List[T], value: T) -> bool:
        for i in 0..self.length:
            if *(self.data + i) == value:
                return true
        return false

    // gives the memory back now (it is also given back automatically at the end of the list's scope)
    function free(self):
        self.clear()
        if self.data != null:
            release(self.data)
        self.data = null
        self.capacity = 0

    operator destruct(self):
        self.free()
