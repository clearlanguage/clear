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
   garbage collector, no hidden allocations, no exceptions. If something
   allocates, you can see it in the source; cleanup happens at the end of a
   scope, where you can see it too. Dynamic dispatch exists only for classes
   in a hierarchy; every other class is exactly its fields.
3. **Control when you ask for it.** Pointers, explicit layout, `sizeof`,
   casts and C interop (`declare`) are first class, never "unsafe" escape
   hatches bolted on later.
4. **Safe by default where it is free.** Checks that cost nothing at run
   time (types, initialization, constant bounds) are always on. Checks that
   cost something (bounds, null) are on in debug builds and can be turned
   off for release.
5. **One obvious way.** Prefer one well-designed feature over two
   overlapping ones. `operator add`, `operator get` … are *the* way to
   customise operators.

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

for x in numbers:          // arrays, and classes with __len__/__getitem__
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

- [x] `for … in` over ranges (`a..b`, `a..=b`), fixed arrays, and any
      class with `__len__` + `__getitem__` (lowered to an index loop).
- [x] `defer` (LIFO, runs on every exit path: return, break, continue).
- [x] `switch` / `case` / `default` (no fallthrough; comma lists).
- [x] `enum` with explicit or automatic values, scoped (`Color.Red`),
      never mixing with plain integers without `as`.
- [x] `print(...)`: built-in, variadic, picks the format from each
      argument's type. Lowered to a single `printf` call at compile time, so
      it costs no more than writing the format string by hand.
- [x] `const` locals/globals with compile-time evaluation (usable in array
      sizes and case labels).
- [x] Char literals (`'a'`) as `int8`.
- [x] Top-level declarations usable before the line that defines them.
- [x] Checked calls: argument count and implicit conversions.

## Phase 3: Types and abstraction

- [x] Python-style operator overloading through dunder methods:
      `__add__ __sub__ __mul__ __div__ __mod__ __eq__ __ne__ __lt__ __le__
      __gt__ __ge__ __getitem__ __setitem__ __len__`.
- [x] Construction: `Point(1, 2)` (memberwise, dataclass style), field
      defaults, `__init__` running on a stack value.
- [x] Generic functions (`function max[T](a: T, b: T) -> T`), with type
      arguments inferred through pointers, arrays and generic classes.
- [x] Methods of generic classes are only analysed when used.
- [x] Destructors (`operator destruct`) called at scope exit, with moves.
- [x] Slices `[]T` (pointer + length) for passing arrays of any length;
      bounds-checked in debug builds.
- [x] Optionals `?T`: `none`, `x is none`, `x.value`, `x.value_or(d)`, `case some(v)`.
- [x] Tagged unions / variants: enum cases with data, matched by `switch` (checked to be exhaustive); plain `union`.
- [x] Traits with static dispatch: `trait`, `class C(Trait)`, generic constraints `[T: Trait]`.
- [x] Lambdas and function values (`function(int) -> int`); capturing lambdas become small objects with `__call__`.
- [x] Generic methods (type parameters on a method of a class).
- [x] An owned, length-tracked `String` type with `+` and comparisons.

## Phase 4: Performance

Clear's promise is C-level speed, so this is measured, not assumed.

- [x] All `alloca`s in the entry block.
- [x] Non-exported functions and globals get internal linkage in
      executables so LLVM can inline and remove them; functions are
      `nounwind`.
- [x] The optimizer runs with the target machine's cost model (this took
      matrix multiply from 1.7x to 1.0x of `clang -O3`).
- [x] `--native` / `--cpu=<name>` targets; `clearc run` uses the host CPU.
- [x] Benchmarks in `Benchmarks/` comparing against equivalent C, reported
      in CI.
- [ ] Struct/array literals built in registers instead of `memcpy` from a
      global constant (only matters at -O0).
- [ ] Debug info (`DebugInfo = true`) so gdb/lldb work.
- [x] Run-time checks (bounds, division, overflow, null, `none`) in debug builds; `--checks` / `--no-checks`.
- [ ] Compiler speed: parse files in parallel; reduce `shared_ptr` churn
      in the AST.

## Phase 5: Standard library and tooling

- [x] `Standard/` rewritten in current syntax and tested: `math`, `memory`,
      `list` (`List[T]`), `string` (C string helpers).
- [x] Imports are followed automatically; `import "math"` finds the
      standard library; files can use each other's functions, classes and
      globals.
- [x] `Map[K, V]`, `io` (files, stdin), an owned `String`.
- [ ] `clearc fmt` formatter.
- [ ] Language server (diagnostics + go-to-definition) built on the same
      front end.
- [x] Package manager: `clear.toml`, git/path dependencies, `clear.lock`, `clearc new/add/fetch/update`.

## Phase 6: A full language

Everything here follows the same rules: visible cost, opt-in dynamism, checked at compile time where possible.

- [x] Inheritance (`class Dog(Animal)`, base fields first, `*Dog` → `*Animal`), `super.method()`.
- [x] Dynamic dispatch for classes in a hierarchy, automatically (no `virtual`); other classes stay static.
- [x] Properties: `property name(self)` getters and `property name(self, value)` setters.
- [x] Hygienic macros: `macro name(args):`, used as `name!(...)`.
- [x] Generators (`Generator[T]`, `yield`) and `async` / `await` / `Task[T]` on LLVM coroutines, no hidden runtime.
- [x] Tuples, multiple return values, destructuring, default and keyword arguments, `...` unpacking.
- [x] `in`, `len`, `assert`, `**`, `else if`, `str` with value comparison, `hash()`, `__str__`.
- [x] Automatic cleanup: `operator destruct`, moves out of locals, no implicit copies of owning values; `List`/`String`/`Map`/`File` clean up themselves.
- [x] `operator add`, `operator get` … instead of Python's dunder methods; `function init` constructors; bare `self`.
- [x] `operator get` returning a reference: `list[i].field = v` edits in place; loops visit objects in place.
- [x] Type variants: `variant Number: int, float64`, checked reads with `as`, `is`, `switch case int(x)`.
- [x] Move checking: using a moved variable, moving inside a loop, and moving twice in one call are compile errors; writes into temporaries are errors; a warning for pointers into a collection used after it changes.
- [x] Lambdas borrow owning values, `move lambda` moves them in; generators and tasks are owned values cleaned up like the rest (also when abandoned mid-way).
- [x] Internal compiler errors are reported with the source line being compiled and a stack trace.
- [x] Slices `[]T` (pointer + length): `xs[a:b]`, `[]T` parameters take arrays, lists and slices.
- [x] Generic methods (type parameters on a method); `List.map/filter/sort/sort_by`.
- [x] Optionals: `if r:` narrowing, `if not r: return`, `??`, `:=`, `?.`.
- [x] Copies skipped where unobservable: last use moves, read-only values look at the original; `--copies` lists the rest.
- [x] `str` is a pointer and a length: `text[a:b]` views without copying, `String` turns into `str` for free, C gets a checked `char*`.
- [ ] Debug info for gdb/lldb; `clearc fmt`; a language server.
- [ ] A package registry (today dependencies are git URLs or paths).

## Out of scope (on purpose)

- Garbage collection, exceptions, implicit heap allocation, runtime
  reflection. These conflict with the "no hidden cost" rule.
