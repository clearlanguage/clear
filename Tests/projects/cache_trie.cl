// Project (a): LRU cache + trie autocomplete + union-find
// Ownership probes: String keys/values, operator copy/destruct counting,
// references from operator get, nested containers, slices, generators.

let live_blobs = 0
let blob_copies = 0

class Blob:
    label: String
    data: List[String]

    function init(self, label: str, n: int):
        self.label = String(label)
        for i in 0..n:
            self.data.push(String(label) + "#" + from_int(i))
        live_blobs += 1

    operator copy(self) -> Blob:
        blob_copies += 1
        live_blobs += 1
        return Blob { String(self.label), self.data }

    operator destruct(self):
        live_blobs -= 1

// ---------- LRU cache: Map from key to slot, slots form a doubly linked list ----------
class Slot[K, V]:
    key: K
    value: V
    prev: int64
    next: int64

class LRU[K, V]:
    capacity: int64
    index: Map[K, int64]
    slots: List[Slot[K, V]]
    free: List[int64]
    head: int64 = -1      // most recently used
    tail: int64 = -1      // least recently used
    evicted: List[K]

    function init(self, capacity: int64):
        self.capacity = capacity
        self.head = -1
        self.tail = -1

    function unlink(self, i: int64):
        let p = self.slots[i].prev
        let n = self.slots[i].next
        if p >= 0:
            self.slots[p].next = n
        else:
            self.head = n
        if n >= 0:
            self.slots[n].prev = p
        else:
            self.tail = p

    function push_front(self, i: int64):
        self.slots[i].prev = -1
        self.slots[i].next = self.head
        if self.head >= 0:
            self.slots[self.head].prev = i
        self.head = i
        if self.tail < 0:
            self.tail = i

    function put(self, key: K, value: V):
        if i := self.index.get(key):
            self.slots[i].value = value
            self.unlink(i)
            self.push_front(i)
            return
        if len(self.index) == self.capacity:
            let victim = self.tail
            self.unlink(victim)
            self.evicted.push(self.slots[victim].key)
            self.index.remove(self.slots[victim].key)
            self.free.push(victim)
        if len(self.free) > 0:
            let i = self.free.pop()
            self.slots[i].key = key
            self.slots[i].value = value
            self.index[key] = i
            self.push_front(i)
        else:
            let i = len(self.slots)
            self.slots.push(Slot[K, V](key, value, -1, -1))
            self.index[key] = i
            self.push_front(i)

    // a reference to the cached value; marks it as recently used
    operator get(self, key: K) -> *V:
        let i = self.index[key]
        self.unlink(i)
        self.push_front(i)
        return &self.slots[i].value

    // a reference without touching the recency order
    function peek(self, key: K) -> *V:
        return &self.slots[self.index[key]].value

    operator contains(self, key: K) -> bool:
        return key in self.index

    operator len(self) -> int64:
        return len(self.index)

    // most recent first
    operator iterate(self) -> Generator[K]:
        let i = self.head
        while i >= 0:
            yield self.slots[i].key
            i = self.slots[i].next

// ---------- Trie ----------
class TrieNode:
    terminal: bool
    count: int
    kids: Map[int8, TrieNode]

class Trie:
    root: TrieNode
    words: int64

    function add(self, word: str):
        let node = &self.root
        for c in word:
            if c not in node.kids:
                node.kids[c] = TrieNode(false, 0, Map[int8, TrieNode]())
            node = &node.kids[c]
        if not node.terminal:
            self.words += 1
        node.terminal = true
        node.count += 1

    function find(self, prefix: str) -> ?*TrieNode:
        let node = &self.root
        for c in prefix:
            if c not in node.kids:
                return none
            node = &node.kids[c]
        return node

    function complete(self, prefix: str) -> Generator[String]:
        if node := self.find(prefix):
            for w in collect(node, String(prefix)):
                yield w

function collect(node: *TrieNode, sofar: String) -> Generator[String]:
    if node.terminal:
        yield sofar
    let letters = List[int8]()
    for c in node.kids:
        letters.push(c)
    letters.sort()
    for c in letters:
        let next = sofar
        next.push(c)
        for w in collect(&node.kids[c], next):
            yield w

// ---------- Union-find over any hashable key ----------
class UnionFind[T]:
    ids: Map[T, int64]
    names: List[T]
    parent: List[int64]
    size: List[int64]

    function id(self, x: T) -> int64:
        if i := self.ids.get(x):
            return i
        let i = len(self.parent)
        self.ids[x] = i
        self.names.push(x)
        self.parent.push(i)
        self.size.push(1)
        return i

    function root(self, i: int64) -> int64:
        let r = i
        while self.parent[r] != r:
            r = self.parent[r]
        let j = i
        while self.parent[j] != r:
            let n = self.parent[j]
            self.parent[j] = r
            j = n
        return r

    function merge(self, a: T, b: T) -> bool:
        let ra = self.root(self.id(a))
        let rb = self.root(self.id(b))
        if ra == rb:
            return false
        if self.size[ra] < self.size[rb]:
            ra, rb = rb, ra
        self.parent[rb] = ra
        self.size[ra] += self.size[rb]
        return true

    function same(self, a: T, b: T) -> bool:
        return self.root(self.id(a)) == self.root(self.id(b))

    function groups(self) -> List[List[T]]:
        let by_root = Map[int64, List[T]]()
        let order = List[int64]()
        for i in 0..len(self.names):
            let r = self.root(i)
            if r not in by_root:
                by_root[r] = List[T]()
                order.push(r)
            by_root[r].push(self.names[i])
        let out = List[List[T]]()
        for r in order:
            out.push(by_root[r])
        return out

// the words of a text, as views into it (no copies)
function words_of(text: str) -> Generator[str]:
    let start: int64 = 0
    for i in 0..=len(text):
        if i == len(text) or text[i] == ' ':
            if i > start:
                yield text[start:i]
            start = i + 1

function join(xs: []String) -> String:
    let s = String()
    for x in xs:
        if len(s) > 0:
            s.append(",")
        s.append(x)
    return s

function test_lru():
    print("== LRU[String, Blob]")
    let c = LRU[String, Blob](3)
    c.put(String("a"), Blob("a", 1))
    c.put(String("b"), Blob("b", 2))
    c.put(String("c"), Blob("c", 3))
    print("len", len(c), "live", live_blobs)
    c[String("a")].data.push(String("touched"))      // reference: changes the cached blob
    c.put(String("d"), Blob("d", 1))                 // evicts b (least recently used)
    let order = List[String]()
    for k in c:
        order.push(k)
    print("order", join(order), "evicted", join(c.evicted))
    print("has b", String("b") in c, "has a", String("a") in c)
    print("a data", join(c[String("a")].data))
    let snapshot = c[String("c")]                    // a copy (operator copy runs)
    snapshot.data.push(String("only in copy"))
    print("c in cache", len(c[String("c")].data), "copy", len(snapshot.data))
    c.put(String("c"), Blob("c2", 0))                 // replace value in place
    c.put(String("e"), Blob("e", 0))
    c.put(String("f"), Blob("f", 0))
    let order2 = List[String]()
    for k in c:
        order2.push(k)
    print("order", join(order2), "evicted", join(c.evicted[1:]))
    print("c label", c[String("c")].label, "len", len(c))

function test_lru_ints():
    print("== LRU[int, String]")
    let c = LRU[int, String](2)
    for i in 0..10:
        c.put(i % 4, from_int(i))
        if i % 3 == 0 and (i % 4) in c:
            c[i % 4].append("!")
    let line = String()
    for k in c:
        if len(line) > 0:
            line.append(" ")
        line.append(from_int(k))
        line.append("=")
        line.append(c.peek(k))
    print(line)

function test_trie():
    print("== Trie")
    let t = Trie(TrieNode(false, 0, Map[int8, TrieNode]()), 0)
    let text = String("car cart carbon care cat dog do dot card car cab")
    for w in words_of(text):
        t.add(w)
    print("distinct", t.words)
    for prefix in ["car", "do", "ca", "z", ""]:
        let found = List[String]()
        for w in t.complete(prefix):
            found.push(w)
        print("complete", prefix, "->", len(found), "[" + join(found[:]) + "]")
    if n := t.find("car"):
        print("car count", n.count, "kids", len(n.kids))
    let first_two = List[String]()
    for w in t.complete("c"):
        first_two.push(w)
        if len(first_two) == 2:
            break
    print("first two", join(first_two))

function test_union_find():
    print("== UnionFind[String]")
    let uf = UnionFind[String]()
    let pairs = [("ada", "bob"), ("cy", "dee"), ("bob", "cy"), ("eve", "fay"), ("gus", "gus"), ("ada", "dee")]
    for p in pairs:
        let a, b = p
        print("merge", a, b, uf.merge(String(a), String(b)))
    print("same ada dee", uf.same(String("ada"), String("dee")), "same ada eve", uf.same(String("ada"), String("eve")))
    for g in uf.groups():
        print(" group", len(g), join(g))
    let ints = UnionFind[int]()
    for i in 0..10:
        ints.merge(i, (i * 3) % 10)
    print("int groups", len(ints.groups()))

function main() -> int32:
    test_lru()
    test_lru_ints()
    test_trie()
    test_union_find()
    print("blobs live", live_blobs, "copies", blob_copies)
    return 0

// expect:
// == LRU[String, Blob]
// len 3 live 3
// order d,a,c evicted b
// has b false has a true
// a data a#0,touched
// c in cache 3 copy 4
// order f,e,c evicted d,a
// c label c2 len 3
// == LRU[int, String]
// 1=9! 0=8
// == Trie
// distinct 10
// complete car -> 5 [car,carbon,card,care,cart]
// complete do -> 3 [do,dog,dot]
// complete ca -> 7 [cab,car,carbon,card,care,cart,cat]
// complete z -> 0 []
// complete  -> 10 [cab,car,carbon,card,care,cart,cat,do,dog,dot]
// car count 2 kids 4
// first two cab,car
// == UnionFind[String]
// merge ada bob true
// merge cy dee true
// merge bob cy true
// merge eve fay true
// merge gus gus false
// merge ada dee false
// same ada dee true same ada eve false
//  group 4 ada,bob,cy,dee
//  group 2 eve,fay
//  group 1 gus
// int groups 4
// blobs live 0 copies 4
