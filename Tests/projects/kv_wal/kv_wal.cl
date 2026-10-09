// The core of a key-value store with a write-ahead log, all in memory: records are encoded
// as checksummed lines and appended to a log String, applied to a Map, replayed into a fresh
// store, and checked against a plain Map model after a pseudo-random workload with
// transactions. Then a corrupted byte, a torn last line and a compaction are replayed.
// Exercises enums with String payloads through an alias, array literals of owning values
// passed as a slice, optionals narrowed by `if not`, Map[String, String], and copies.
import "kv_record" as kc
import "kv_text" as tx

class Store:
    data: Map[String, String]
    log: String
    records: int64 = 0

    function apply(self, r: *kc.Record):
        switch *r:
            case Set(key, value):
                self.data[key] = value
            case Del(key):
                self.data.remove(key)
            case Clear:
                self.data.clear()

    // logs the records with one append, then applies them
    function commit(self, records: []kc.Record):
        for r in records:
            self.log += kc.encode(&r)
        for r in records:
            self.apply(&r)
        self.records += len(records)

    function set(self, key: str, value: str):
        self.commit([kc.Record.Set(key = String(key), value = String(value))])

    function delete(self, key: str):
        self.commit([kc.Record.Del(key = String(key))])

class Replayed:
    store: Store
    good: int64 = 0
    damaged: int64 = 0

// rebuilds a store from a log; a damaged line is skipped, a last line without '\n' is torn
function replay(log: str) -> Replayed:
    let out = Replayed()
    let start: int64 = 0
    for i in 0..len(log):
        if log[i] == '\n':
            if rec := kc.decode(log[start:i]):
                out.store.apply(&rec)
                out.good++
            else:
                out.damaged++
            start = i + 1
    if start < len(log):
        out.damaged++
    return out

function same(a: *Map[String, String], b: *Map[String, String]) -> bool:
    if len(*a) != len(*b):
        return false
    for k in *a:
        let other = b.get(k)
        if not other or other != (*a)[k]:
            return false
    return true

class Rng:
    state: uint64 = 88172645463325252

    function below(self, n: int) -> int:
        self.state = self.state * 6364136223846793005 + 1442695040888963407
        return ((self.state >> 33) % n as uint64) as int

const VALUES: [5; str] = {"plain", "tab\there", "two\nlines", "back\\slash", ""}

function main() -> int32:
    print(kc.FORMAT, tx.hex(kc.checksum("")), tx.hex(kc.checksum("a")))
    let r = kc.Record.Set(key = String("we\tird"), value = String("multi\nline \\ value"))
    let line = kc.encode(&r)
    print(tx.escape("a\tb\\n"), len(line), line[0:14] == "S\twe\\tird\tmult")
    if back := kc.decode(line[0:len(line) - 1]):
        switch back:
            case Set(key, value):
                print("decoded set", key == "we\tird", value == "multi\nline \\ value")
            default:
                print("decoded the wrong kind")
    print(kc.decode("S\tk\tv\t0000000000000000") is none, kc.decode("garbage") is none, kc.decode("S\tk\\q\tv\t" + tx.hex(kc.checksum("S\tk\\q\tv"))) is none)

    // workload: the store goes through records, the model is a plain map
    let store = Store()
    let model = Map[String, String]()
    let rng = Rng()
    let pending = List[kc.Record]()
    let staged = Map[String, ?String]()
    let in_txn = false
    let commits = 0
    let rollbacks = 0
    for step in 0..600:
        let key = "k" + from_int(rng.below(25))
        let roll = rng.below(100)
        if roll < 50:
            let value = from_int(step) + " " + VALUES[rng.below(5)]
            if in_txn:
                pending.push(kc.Record.Set(key = key, value = value))
                staged[key] = value
            else:
                store.set(key, value)
                model[key] = value
        else if roll < 75:
            if in_txn:
                pending.push(kc.Record.Del(key = key))
                staged[key] = none
            else:
                store.delete(key)
                model.remove(key)
        else if roll < 77 and not in_txn:
            store.commit([kc.Record.Clear])
            model.clear()
        else if roll < 85 and not in_txn:
            in_txn = true
        else if roll < 92 and in_txn:
            if rng.below(3) > 0:
                store.commit(pending[:])
                for k in staged:
                    if v := staged[k]:
                        model[k] = v
                    else:
                        model.remove(k)
                commits++
            else:
                rollbacks++
            pending.clear()
            staged.clear()
            in_txn = false
    print("records", store.records, "keys", len(store.data), "commits", commits, "rollbacks", rollbacks)
    print("store matches model:", same(&store.data, &model))
    print("log checksum", tx.hex(kc.checksum(store.log)))

    let again = replay(store.log)
    print("replay good", again.good, "damaged", again.damaged, "matches:", same(&again.store.data, &model))

    // one damaged byte in the middle of the log loses exactly that record
    let bad = store.log
    let mid = len(bad) / 2
    bad[mid] = when bad[mid] == 'x' use 'y' otherwise 'x'
    let hurt = replay(bad)
    print("corrupt good", hurt.good, "damaged", hurt.damaged)

    // a torn write: the last line has no newline
    let torn = store.log + "S\tbroken"
    let after_tear = replay(torn)
    print("torn good", after_tear.good, "damaged", after_tear.damaged, "matches:", same(&after_tear.store.data, &model))

    // compaction: one Set per live key, sorted
    let keys = List[String]()
    for k in model:
        keys.push(k)
    keys.sort()
    let snapshot = Store()
    for k in keys:
        snapshot.set(k, model[k])
    let compacted = replay(snapshot.log)
    print("compacted", compacted.good, "lines, matches:", same(&compacted.store.data, &model))
    print("first keys", keys[0:3], "value", model[keys[0]] == store.data[keys[0]])
    return 0

// expect:
// kv1 cbf29ce484222325 af63dc4c8601ec8c
// a\tb\\n 48 true
// decoded set true true
// true true true
// records 358 keys 16 commits 26 rollbacks 15
// store matches model: true
// log checksum b19b0bef576e5222
// replay good 358 damaged 0 matches: true
// corrupt good 357 damaged 1
// torn good 358 damaged 1 matches: true
// compacted 16 lines, matches: true
// first keys [k1, k10, k11] value true
