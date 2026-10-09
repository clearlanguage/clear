// map: a hash table from keys to values, like Python's dict but with one key type and one value type.
//
//     import "map"
//     let ages = Map[str, int]()
//     defer ages.free()
//     ages["ada"] = 36
//     if "ada" in ages:
//         print(ages["ada"])
//     for name in ages:              // keys, in no particular order
//         print(name, ages[name])
//
// Keys are hashed with the built-in hash() (numbers, enums, pointers, str, or a class with operator hash)
// and compared with ==. Open addressing with linear probing; the table doubles when it is 3/4 full.

import "memory"

class Map[K, V]:
    keys: *K
    values: *V
    states: *uint8          // 0 empty, 1 in use, 2 removed
    length: int64
    capacity: int64
    removed: int64

    // the slot holding key, or -1
    function find(self, key: *K) -> int64:
        if self.capacity == 0:
            return -1
        let mask = self.capacity - 1
        let slot = (hash(*key) as int64) & mask
        while self.states[slot] != 0:
            if self.states[slot] == 1 and *(self.keys + slot) == *key:
                return slot
            slot = (slot + 1) & mask
        return -1

    function grow(self):
        let old_keys = self.keys
        let old_values = self.values
        let old_states = self.states
        let old_capacity = self.capacity

        self.capacity = when old_capacity == 0 use 8 otherwise old_capacity * 2
        self.keys = allocate[K](self.capacity)
        self.values = allocate[V](self.capacity)
        self.states = allocate[uint8](self.capacity)
        self.length = 0
        self.removed = 0

        // the entries move to the new table (take hands them over without copying)
        for i in 0..old_capacity:
            if old_states[i] == 1:
                self.insert(take(old_keys + i), take(old_values + i))

        if old_capacity > 0:
            release(old_keys)
            release(old_values)
            release(old_states)

    // map[key] = value
    operator set(self, key: K, value: V):
        self.insert(key, value)

    function insert(self, key: K, value: V):
        if (self.length + self.removed + 1) * 4 > self.capacity * 3:
            self.grow()

        let mask = self.capacity - 1
        let slot = (hash(key) as int64) & mask
        let reuse: int64 = -1

        while self.states[slot] != 0:
            if self.states[slot] == 1 and *(self.keys + slot) == key:
                destroy(self.values + slot)            // the value it replaces
                *(self.values + slot) = value
                return
            if self.states[slot] == 2 and reuse < 0:
                reuse = slot
            slot = (slot + 1) & mask

        if reuse >= 0:
            slot = reuse
            self.removed -= 1

        *(self.keys + slot) = key
        *(self.values + slot) = value
        self.states[slot] = 1
        self.length += 1

    // map[key] is the value itself (checked like a list index): read it, change it, map[key] += 1
    operator get(self, key: K) -> *V:
        let slot = self.find(&key)
        assert slot >= 0, "key not in Map"
        return self.values + slot

    // the value, or none (a copy, so for values that own memory use map[key] or `key in map`)
    function get(self, key: K) -> ?V:
        if self.find(&key) < 0:
            return none
        return self[key]

    function get_or(self, key: K, fallback: V) -> V:
        if self.find(&key) < 0:
            return fallback
        return self[key]

    operator contains(self, key: K) -> bool:
        return self.find(&key) >= 0

    // true when the key was there
    function remove(self, key: K) -> bool:
        let slot = self.find(&key)
        if slot < 0:
            return false
        destroy(self.keys + slot)
        destroy(self.values + slot)
        self.states[slot] = 2
        self.length -= 1
        self.removed += 1
        return true

    operator len(self) -> int64:
        return self.length

    function is_empty(self) -> bool:
        return self.length == 0

    function clear(self):
        for i in 0..self.capacity:
            if self.states[i] == 1:
                destroy(self.keys + i)
                destroy(self.values + i)
            self.states[i] = 0
        self.length = 0
        self.removed = 0

    // `for key in map`: the keys, in no particular order
    operator iterate(self) -> Generator[K]:
        for slot in 0..self.capacity:
            if self.states[slot] == 1:
                yield *(self.keys + slot)

    // gives the memory back now (it is also given back automatically at the end of the map's scope)
    function free(self):
        self.clear()
        if self.capacity > 0:
            release(self.keys)
            release(self.values)
            release(self.states)
        self.keys = null
        self.values = null
        self.states = null
        self.length = 0
        self.capacity = 0
        self.removed = 0

    operator destruct(self):
        self.free()

    // let b = a gives b its own map (and its own copies of the keys and values)
    operator copy(self) -> Map[K, V]:
        let result = Map[K, V]()
        for slot in 0..self.capacity:
            if self.states[slot] == 1:
                result.insert(clone(self.keys + slot), clone(self.values + slot))
        return result

    function copy(self) -> Map[K, V]:
        return clone(self)
