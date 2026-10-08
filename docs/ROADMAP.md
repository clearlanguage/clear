# Clear Roadmap

This is the working plan for growing Clear from an early prototype into a
language people can rely on. It is ordered: each phase builds on the one
before it, and nothing in a later phase should start until the earlier
phase's tests pass.

## The ethos (the test every feature must pass)

Clear is **"read like Python, run like C"**. Concretely:

1. **Readable first.** Indentation blocks, words over symbols where it reads
   better (`and`, `or`, `not`, `when … use … otherwise`, `as`, `is`).
   A newcomer should be able to guess what a line does.
2. **No hidden cost.** Every construct maps to obvious machine code. No
   garbage collector, no hidden allocations, no implicit virtual dispatch,
   no exceptions. If something allocates, you can see it in the source.
3. **Control when you ask for it.** Pointers, explicit layout, `sizeof`,
   casts and C interop (`declare`) are first class, never "unsafe" escape
   hatches bolted on later.
4. **Safe by default where it is free.** Checks that cost nothing at run
   time (types, initialization, constant bounds) are always on. Checks that
   cost something (bounds, null) are on in debug builds and can be turned
   off for release.
5. **One obvious way.** Prefer one well-designed feature over two
   overlapping ones. Python-style dunder methods (`__add__`, `__getitem__`)
   are *the* way to customise operators.

## Phase 0: Foundation (make the compiler trustworthy)

Without these, every later change is guesswork.

- [x] Fix the shutdown crash (LLVM context freed before its modules).
- [x] **Test suite**: `Tests/<area>/<name>.cl` plus a `# expect:` block holding
      the expected stdout. A runner script compiles and runs each test, and
      `ctest` drives it.
- [x] **CI that actually builds**: install LLVM 18 + Clang on Ubuntu, build,
      run the tests. Drop the gcc/MSVC jobs (the README already requires
      Clang).
- [x] Exit with a non-zero status when compilation fails; print each
      diagnostic once.
- [x] Run the optimizer **before** emitting the object file. Today the
      optimizer runs after the `.o` is written, so release builds are never
      optimized.
- [x] `clearc run file.cl` / `clearc build file.cl` for single files, so a
      `build.toml` is only needed for real projects.

## Phase 1: Core correctness (make what exists actually work)

Everything here is already "in the language" on paper but fails today.

- [x] Operator precedence that matches Python/C intuition:
      `or` < `and` < `not` < comparisons < `|` < `^` < `&` < shifts <
      `+ -` < `* / %` < unary < postfix. Today `a < b and c < d` mis-parses.
- [x] `and`, `or`, `not` and the bitwise operators `& | ^ ~ << >>` in the
      semantic pass; `&= |= ^= <<= >>=`.
- [x] Unary minus.
- [x] `break` and `continue`.
- [x] Method calls on values (`v.sum()` currently hangs the compiler).
- [x] Generic class instantiation from a struct literal (`Box { 7 }`).
- [x] C variadic calls promote `float32` to `float64` (so `printf("%f")` works).
- [x] Float literals default to `float64`, like Python.
- [x] Implicit, *lossless* conversions are inserted by the semantic pass
      (int widening, int → float). Lossy conversions need `as`.
- [x] Indexing a class without `__getitem__` is a normal diagnostic, not a
      compiler crash (`operator_get`/`operator_set` are now `__getitem__` /
      `__setitem__`).
- [x] Readable diagnostics with file:line:column and the offending source
      line; no crashes when reporting errors without a location.

## Phase 2: Control flow and everyday ergonomics

```clear
for i in 0..10:            // exclusive range
    print(i)

for x in numbers:          // any fixed array
    total += x

defer free(buffer)         // runs when the scope exits

switch command:
    case 1, 2:
        start()
    case 3:
        stop()
    default:
        print("unknown")

enum Color:
    Red
    Green
    Blue

print("total:", total, "avg:", total / count)   // built-in, type-aware
```

- [ ] `for … in` over ranges (`a..b`, `a..=b`) and fixed arrays.
- [ ] `defer` (LIFO, runs on every exit path: return, break, continue).
- [ ] `switch` / `case` / `default` (no fallthrough; comma lists).
- [ ] `enum` with explicit or automatic values, scoped (`Color.Red`).
- [ ] `print(...)`: built-in, variadic, picks the format from each
      argument's type. Lowered to a single `printf` call at compile time, so
      it costs no more than writing the format string by hand.
- [ ] `const` locals/globals with compile-time evaluation.
- [ ] Char literals (`'a'`) as `int8`.

## Phase 3: Types and abstraction

- [ ] Python-style operator overloading through dunder methods:
      `__add__`, `__sub__`, `__mul__`, `__div__`, `__eq__`, `__lt__`, …,
      `__getitem__`, `__setitem__`. The current `operator_get` and
      `operator_set` are renamed to these.
- [ ] Constructors and destructors (`__construct__`, `__destruct__`),
      destructors called at scope exit (deterministic, like RAII).
- [ ] Generic functions (`function max[T](a: T, b: T) -> T`).
- [ ] Slices `[]T` (pointer + length) for passing arrays of any length;
      bounds-checked in debug builds.
- [ ] Optionals `?T` with `if value is some:` style unwrapping.
- [ ] Tagged unions / variants.
- [ ] Traits/interfaces with static dispatch (monomorphised, zero cost).
- [ ] Lambdas (non-capturing first, then capturing by explicit list).

## Phase 4: Performance

Clear's promise is C-level speed, so this is measured, not assumed.

- [ ] All `alloca`s in the entry block (today array literals inside loops
      grow the stack every iteration).
- [ ] Struct/array literals built in registers instead of `memcpy` from a
      global constant.
- [ ] Non-exported functions get internal linkage so LLVM can inline and
      remove them; mark functions `nounwind`.
- [ ] Honour `BuildConfig` CPU features / `-march=native`-style targets.
- [ ] Debug info (`DebugInfo = true`) so gdb/lldb work.
- [ ] Benchmarks in `Benchmarks/` comparing against equivalent C, run in CI
      to catch regressions.
- [ ] Compiler speed: parse files in parallel; reduce `shared_ptr` churn
      in the AST.

## Phase 5: Standard library and tooling

- [ ] Rewrite `Standard/` in current syntax and test it: `math`, `memory`
      (allocator), `string` (owned, length-tracked), `list[T]` (dynamic
      array), `map[K, V]`, `io` (files, stdin).
- [ ] Module search path so `import "math"` finds the standard library.
- [ ] `clearc fmt` formatter.
- [ ] Language server (diagnostics + go-to-definition) built on the same
      front end.
- [ ] Package manager.

## Out of scope (on purpose)

- Garbage collection, exceptions, implicit heap allocation, runtime
  reflection. These conflict with the "no hidden cost" rule.
