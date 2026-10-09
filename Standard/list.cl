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

    // let b = a gives b its own list (and its own copies of the items)
    operator copy(self) -> List[T]:
        let result = List[T]()
        for i in 0..self.length:
            result.push(clone(self.data + i))
        return result

    function copy(self) -> List[T]:
        return clone(self)

    // a new list with f applied to every item: words.map(lambda w: len(w))
    function map[U](self, f: function(T) -> U) -> List[U]:
        let result = List[U]()
        for i in 0..self.length:
            result.push(f(self[i]))
        return result

    // a new list with the items keep says yes to: numbers.filter(lambda n: n % 2 == 0)
    function filter[F](self, keep: F) -> List[T]:
        let result = List[T]()
        for i in 0..self.length:
            if keep(self[i]):
                result.push(self[i])
        return result

    // smallest first (the items need <); equal items keep their order
    function sort(self):
        let order = self.sorted_order(lambda a, b: *(self.data + a) < *(self.data + b))
        self.reorder(&order)

    // by what key gives for each item, smallest first: names.sort_by(lambda n: len(n))
    function sort_by[K](self, key: function(T) -> K):
        let keys = self.map(key)
        let order = self.sorted_order(lambda a, b: keys[a] < keys[b])
        self.reorder(&order)

    // the positions of the items in sorted order (a stable merge sort); before(a, b): item a goes before item b
    function sorted_order[F](self, before: F) -> List[int64]:
        let order = List[int64]()
        let spare = List[int64]()
        for i in 0..self.length:
            order.push(i)
            spare.push(i)
        let width: int64 = 1
        while width < self.length:
            let start: int64 = 0
            while start < self.length:
                let middle = when start + width < self.length use start + width otherwise self.length
                let end = when start + 2 * width < self.length use start + 2 * width otherwise self.length
                let left = start
                let right = middle
                for out in start..end:
                    if left < middle and (right >= end or not before(order[right], order[left])):
                        spare[out] = order[left]
                        left += 1
                    else:
                        spare[out] = order[right]
                        right += 1
                start += 2 * width
            for i in 0..self.length:
                order[i] = spare[i]
            width *= 2
        return order

    // puts the items in the given order by moving their bytes (nothing is copied or cleaned up)
    function reorder(self, order: *List[int64]):
        if self.length == 0:
            return
        let moved = allocate[T](self.capacity)
        for i in 0..self.length:
            copy(moved + i, self.data + order[i], 1)
        release(self.data)
        self.data = moved
