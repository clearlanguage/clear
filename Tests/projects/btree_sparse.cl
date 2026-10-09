// Project (b): a small B-tree (minimum degree 2, i.e. 2-3-4 tree) with String keys and
// List[String] values, insert / delete / range queries via generators,
// plus a sparse matrix keyed by a Cell class.

const T = 2           // minimum degree: nodes hold T-1 .. 2T-1 keys

class BNode:
    keys: List[String]
    vals: List[List[String]]
    kids: List[BNode]

    function leaf(self) -> bool:
        return len(self.kids) == 0

function new_node() -> BNode:
    return BNode(List[String](), List[List[String]](), List[BNode]())

class BTree:
    root: BNode
    count: int64

    function get(self, key: str) -> ?*List[String]:
        let node = &self.root
        while true:
            let i: int64 = 0
            while i < len(node.keys) and node.keys[i] < key:
                i += 1
            if i < len(node.keys) and node.keys[i] == key:
                return &node.vals[i]
            if node.leaf():
                return none
            node = &node.kids[i]
        return none

    function add(self, key: str, value: str):
        if vals := self.get(key):
            vals.push(String(value))
            return
        if len(self.root.keys) == 2 * T - 1:
            let old = self.root
            self.root = new_node()
            self.root.kids.push(old)
            split_child(&self.root, 0)
        let vals = List[String]()
        vals.push(String(value))
        insert_nonfull(&self.root, String(key), vals)
        self.count += 1

    function remove(self, key: str) -> bool:
        if self.get(key) is none:
            return false
        delete_from(&self.root, key)
        if len(self.root.keys) == 0 and not self.root.leaf():
            self.root = self.root.kids.pop()
        self.count -= 1
        return true

    function range(self, lo: str, hi: str) -> Generator[String]:
        for k in walk_range(&self.root, lo, hi):
            yield k

    function height(self) -> int:
        let h = 1
        let node = &self.root
        while not node.leaf():
            node = &node.kids[0]
            h += 1
        return h

    operator iterate(self) -> Generator[String]:
        for k in walk_range(&self.root, "", "\x7f"):
            yield k

// move the full child i of parent into two halves, lifting the middle key
function split_child(parent: *BNode, i: int64):
    let full = &parent.kids[i]
    let right = new_node()
    for j in T..2 * T - 1:
        right.keys.push(full.keys[j])
        right.vals.push(full.vals[j])
    if not full.leaf():
        for j in T..2 * T:
            right.kids.push(full.kids[j])
        while len(full.kids) > T:
            full.kids.pop()
    let mid_key = full.keys[T - 1]
    let mid_val = full.vals[T - 1]
    while len(full.keys) > T - 1:
        full.keys.pop()
        full.vals.pop()
    parent.keys.insert(i, mid_key)
    parent.vals.insert(i, mid_val)
    parent.kids.insert(i + 1, right)

function insert_nonfull(node: *BNode, key: String, vals: List[String]):
    let i: int64 = 0
    while i < len(node.keys) and node.keys[i] < key:
        i += 1
    if node.leaf():
        node.keys.insert(i, key)
        node.vals.insert(i, vals)
        return
    if len(node.kids[i].keys) == 2 * T - 1:
        split_child(node, i)
        if node.keys[i] < key:
            i += 1
    insert_nonfull(&node.kids[i], key, vals)

// CLRS-style delete; every child we descend into has at least T keys
function delete_from(node: *BNode, key: str):
    let i: int64 = 0
    while i < len(node.keys) and node.keys[i] < key:
        i += 1
    if i < len(node.keys) and node.keys[i] == key:
        if node.leaf():
            node.keys.remove(i)
            node.vals.remove(i)
            return
        if len(node.kids[i].keys) >= T:
            let pk, pv = max_entry(&node.kids[i])
            node.keys[i] = pk
            node.vals[i] = pv
            delete_from(&node.kids[i], node.keys[i])
            return
        if len(node.kids[i + 1].keys) >= T:
            let sk, sv = min_entry(&node.kids[i + 1])
            node.keys[i] = sk
            node.vals[i] = sv
            delete_from(&node.kids[i + 1], node.keys[i])
            return
        merge_children(node, i)
        delete_from(&node.kids[i], key)
        return
    if node.leaf():
        return
    if len(node.kids[i].keys) < T:
        if i > 0 and len(node.kids[i - 1].keys) >= T:
            // borrow from the left sibling through the parent
            let left = &node.kids[i - 1]
            let lk = left.keys.pop()
            let lv = left.vals.pop()
            node.kids[i].keys.insert(0, node.keys[i - 1])
            node.kids[i].vals.insert(0, node.vals[i - 1])
            node.keys[i - 1] = lk
            node.vals[i - 1] = lv
            if not node.kids[i - 1].leaf():
                let moved = node.kids[i - 1].kids.pop()
                node.kids[i].kids.insert(0, moved)
        else if i < len(node.keys) and len(node.kids[i + 1].keys) >= T:
            let rk = node.kids[i + 1].keys[0]
            let rv = node.kids[i + 1].vals[0]
            node.kids[i + 1].keys.remove(0)
            node.kids[i + 1].vals.remove(0)
            node.kids[i].keys.push(node.keys[i])
            node.kids[i].vals.push(node.vals[i])
            node.keys[i] = rk
            node.vals[i] = rv
            if not node.kids[i + 1].leaf():
                let moved = node.kids[i + 1].kids[0]
                node.kids[i + 1].kids.remove(0)
                node.kids[i].kids.push(moved)
        else if i < len(node.keys):
            merge_children(node, i)
        else:
            merge_children(node, i - 1)
            i -= 1
    delete_from(&node.kids[i], key)

// kids[i] + keys[i] + kids[i+1] -> kids[i]
function merge_children(node: *BNode, i: int64):
    let right = node.kids[i + 1]
    node.kids.remove(i + 1)
    let left = &node.kids[i]
    left.keys.push(node.keys[i])
    left.vals.push(node.vals[i])
    node.keys.remove(i)
    node.vals.remove(i)
    for j in 0..len(right.keys):
        left.keys.push(right.keys[j])
        left.vals.push(right.vals[j])
    for k in right.kids:
        left.kids.push(k)

function max_entry(node: *BNode) -> (String, List[String]):
    let n = node
    while not n.leaf():
        n = &n.kids[len(n.kids) - 1]
    return n.keys[len(n.keys) - 1], n.vals[len(n.vals) - 1]

function min_entry(node: *BNode) -> (String, List[String]):
    let n = node
    while not n.leaf():
        n = &n.kids[0]
    return n.keys[0], n.vals[0]

function walk_range(node: *BNode, lo: str, hi: str) -> Generator[String]:
    for i in 0..len(node.keys):
        if not node.leaf() and lo < node.keys[i]:
            for k in walk_range(&node.kids[i], lo, hi):
                yield k
        if node.keys[i] >= lo and node.keys[i] <= hi:
            yield node.keys[i]
        if node.keys[i] > hi:
            return
    if not node.leaf():
        for k in walk_range(&node.kids[len(node.kids) - 1], lo, hi):
            yield k

function check(node: *BNode, is_root: bool) -> int:
    // returns the depth; panics if the B-tree rules are broken
    assert len(node.keys) <= 2 * T - 1, "too many keys"
    assert is_root or len(node.keys) >= T - 1, "too few keys"
    assert len(node.keys) == len(node.vals), "keys/vals mismatch"
    for i in 1..len(node.keys):
        assert node.keys[i - 1] < node.keys[i], "keys out of order"
    if node.leaf():
        return 1
    assert len(node.kids) == len(node.keys) + 1, "wrong child count"
    let d = check(&node.kids[0], false)
    for k in node.kids[1:]:
        assert check(&k, false) == d, "uneven depth"
    return d + 1

// ---------- sparse matrix ----------
class Cell:
    r: int
    c: int

    operator equals(self, other: Cell) -> bool:
        return self.r == other.r and self.c == other.c

    operator less(self, other: Cell) -> bool:
        return self.r < other.r or (self.r == other.r and self.c < other.c)

    operator hash(self) -> uint64:
        return (self.r as uint64) * 1000003 + (self.c as uint64)

class Sparse:
    rows: int
    cols: int
    cells: Map[Cell, float64]
    name: String

    operator get(self, r: int, c: int) -> float64:
        return self.cells.get_or(Cell(r, c), 0.0)

    operator set(self, r: int, c: int, v: float64):
        if v == 0.0:
            self.cells.remove(Cell(r, c))
        else:
            self.cells[Cell(r, c)] = v

    // non-zero cells in row-major order
    function entries(self) -> Generator[(Cell, float64)]:
        let order = List[Cell]()
        for cell in self.cells:
            order.push(cell)
        order.sort()
        for cell in order:
            yield (cell, self.cells[cell])

    function transpose(self) -> Sparse:
        let t = Sparse(self.cols, self.rows, Map[Cell, float64](), self.name + "^T")
        for e in self.entries():
            let cell, v = e
            t[cell.c, cell.r] = v
        return t

    operator multiply(self, other: Sparse) -> Sparse:
        let out = Sparse(self.rows, other.cols, Map[Cell, float64](), self.name + "*" + other.name)
        for a in self.entries():
            for b in other.entries():
                if a[0].c == b[0].r:
                    out[a[0].r, b[0].c] = out[a[0].r, b[0].c] + a[1] * b[1]
        return out

function show(m: *Sparse):
    print(m.name, m.rows, "x", m.cols, "nnz", len(m.cells))
    for r in 0..m.rows:
        let line = String(" ")
        for c in 0..m.cols:
            line.append(" ")
            line.append(from_float(m[r, c]))
        print(line)

function keys_of(t: *BTree) -> String:
    let s = String()
    for k in t:
        if len(s) > 0:
            s.append(" ")
        s.append(k)
    return s

function test_btree():
    print("== BTree")
    let t = BTree(new_node(), 0)
    let words = ["kiwi", "apple", "fig", "date", "lime", "plum", "pear", "grape", "melon", "banana", "cherry", "mango", "olive", "quince", "lemon", "nectarine", "apple", "fig"]
    for i in 0..len(words):
        t.add(words[i], "w" + from_int(i))
        check(&t.root, true)
    print("count", t.count, "height", t.height(), "root keys", len(t.root.keys))
    print(keys_of(&t))
    if v := t.get("apple"):
        let joined = String()
        for x in v:
            joined.append(x)
        print("apple ->", joined, len(v))
    print("fig?", t.get("fig") is not none, "zzz?", t.get("zzz") is none)
    let in_range = List[String]()
    for k in t.range("c", "lz"):
        in_range.push(k)
    print("range c..lz", len(in_range), in_range[0], in_range[len(in_range) - 1])
    let firsts = in_range[:3]
    print("first three", firsts[0], firsts[1], firsts[2], len(firsts))
    let backup = t
    for w in ["fig", "kiwi", "apple", "zzz", "mango", "date", "lime", "banana", "cherry"]:
        let ok = t.remove(w)
        check(&t.root, true)
        print("remove", w, ok, "count", t.count, "height", t.height())
    print(keys_of(&t))
    print("backup still", backup.count, keys_of(&backup))
    for w in ["grape", "lemon", "melon", "nectarine", "olive", "pear", "plum", "quince"]:
        t.remove(w)
        check(&t.root, true)
    print("empty", t.count, len(t.root.keys), t.root.leaf())
    for i in 0..200:
        t.add("k" + from_int((i * 37) % 101), "v")
        check(&t.root, true)
    print("bulk", t.count, "height", t.height())
    for i in 0..101:
        if i % 2 == 0:
            t.remove("k" + from_int(i))
    check(&t.root, true)
    let n = 0
    for k in t:
        n += 1
    print("after removing evens", t.count, n)

function test_sparse():
    print("== Sparse")
    let a = Sparse(3, 3, Map[Cell, float64](), "A")
    a[0, 0] = 2.0
    a[0, 2] = 1.0
    a[1, 1] = 3.0
    a[2, 0] = 4.0
    a[2, 2] = 0.5
    a[1, 1] = 0.0        // removes it again
    a[1, 2] = -1.0
    show(&a)
    let at = a.transpose()
    show(&at)
    let p = a * at
    show(&p)
    let total = 0.0
    for e in p.entries():
        total += e[1]
    print("sum of A*A^T", total)
    let copy = p
    copy[0, 0] = 100.0
    print("p00", p[0, 0], "copy00", copy[0, 0], copy.name)

function main() -> int32:
    test_btree()
    test_sparse()
    return 0

// expect:
// == BTree
// count 16 height 3 root keys 1
// apple banana cherry date fig grape kiwi lemon lime mango melon nectarine olive pear plum quince
// apple -> w1w16 2
// fig? true zzz? true
// range c..lz 7 cherry lime
// first three cherry date fig 3
// remove fig true count 15 height 3
// remove kiwi true count 14 height 3
// remove apple true count 13 height 3
// remove zzz false count 13 height 3
// remove mango true count 12 height 3
// remove date true count 11 height 3
// remove lime true count 10 height 3
// remove banana true count 9 height 2
// remove cherry true count 8 height 2
// grape lemon melon nectarine olive pear plum quince
// backup still 16 apple banana cherry date fig grape kiwi lemon lime mango melon nectarine olive pear plum quince
// empty 0 0 true
// bulk 101 height 5
// after removing evens 50 50
// == Sparse
// A 3 x 3 nnz 5
//   2.0 0.0 1.0
//   0.0 0.0 -1.0
//   4.0 0.0 0.5
// A^T 3 x 3 nnz 5
//   2.0 0.0 4.0
//   0.0 0.0 0.0
//   1.0 -1.0 0.5
// A*A^T 3 x 3 nnz 9
//   5.0 -1.0 8.5
//   -1.0 1.0 -0.5
//   8.5 -0.5 16.25
// sum of A*A^T 36.25
// p00 5.0 copy00 100.0 A*A^T
