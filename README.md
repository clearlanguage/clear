
# Clear Programming Language

Clear is a compiled language that **reads like Python and runs like C**. Blocks are indentation, keywords are words (`and`, `or`, `not`, `when … use … otherwise`), and every construct compiles through LLVM to the machine code you would expect, with no garbage collector, hidden allocations or exceptions.

```clear
import "math"
import "list"

class Vec2:
    x: float64
    y: float64

    function __add__(self: *Vec2, other: Vec2) -> Vec2:
        return Vec2(self.x + other.x, self.y + other.y)

    function length(self: *Vec2) -> float64:
        return sqrt(self.x * self.x + self.y * self.y)

function largest[T](values: *List[T]) -> T:
    let best = values[0]
    for value in values:
        best = max(best, value)
    return best

function main() -> int32:
    let v = Vec2(3.0, 4.0) + Vec2(0.0, 0.0)
    print(v, v.length())               // Vec2(x=3.0, y=4.0) 5.0

    let numbers = List[int]()
    defer numbers.free()
    for n in 0..5:
        numbers.push(n * n)
    print(largest(&numbers))           // 16
    return 0
```

The design rules every feature follows are in [docs/ROADMAP.md](docs/ROADMAP.md), together with the plan for what comes next.

---

## Getting started

### Requirements

* Clang 18 or newer with C++23 support (`<print>`), used to build the compiler and to link programs
* LLVM 18 development files (CMake package)
* CMake 3.20+ and Python 3
* Linux or macOS

On Ubuntu 24.04:

```
sudo apt install clang-18 llvm-18-dev libstdc++-14-dev zlib1g-dev libzstd-dev libedit-dev libcurl4-openssl-dev cmake python3
```

### Building

```
git clone https://github.com/clearlanguage/clear.git
cmake -S clear -B build -DCMAKE_BUILD_TYPE=Release \
      -DCMAKE_CXX_COMPILER=clang++-18 -DCMAKE_C_COMPILER=clang-18 \
      -DLLVM_DIR=/usr/lib/llvm-18/lib/cmake/llvm
cmake --build build -j
```

### Using `clearc`

```
clearc run hello.cl              # compile for this machine and run
clearc build hello.cl -O3        # produce ./hello, fully optimized
clearc build hello.cl -o app --native --emit-ir
clearc build my_project/         # a directory with a build.toml
clearc --build_template my_project/
```

| option | meaning |
| --- | --- |
| `-O0` … `-O3` | optimization level (default `-O1`; `-O3` is fastest) |
| `--native`, `--cpu=<name>` | use every instruction this CPU (or the named one) supports; `run` does this by default |
| `--emit-ir` | also write the LLVM IR as a `.ll` file |
| `-o <path>` | output path |
| `-v` | print progress |

Errors point at the exact place in the source:

```
error[E048]: Cannot convert implicitly, the conversion may lose information.
  --> main.cl:6:17
  |
6 |     return half(big)
  |                 ^^^
  = help: Converting ‘int64’ to ‘int32’ needs an explicit cast, for example ‘value as T’.
```

---

## A tour of the language

### Values and types

```clear
let count = 0                  // int32, inferred
let ratio = 2.5                // float64, like Python's float
let small: uint8 = 200         // literals adapt to the declared type
let big: int64 = count         // widening is implicit
let back = big as int32        // narrowing needs `as`
const LIMIT = 64               // folded at compile time
let buffer: [LIMIT; int] = {}  // fixed size arrays (sizes may use consts)
let p: *int = &count           // pointers, *p to dereference
let name = "clear"             // string literals are *int8
```

Built-in types: `int8 … int64`, `uint8 … uint64`, `int` (= `int32`), `uint` (= `uint32`), `float32`, `float64`, `float` (= `float64`, like Python), `bool`, pointers `*T`, arrays `[N; T]`. Array literals may be shorter than the array, the rest is zero: `let grid: [9; int] = {}`.

Conversions that cannot lose information happen automatically (int → wider int, int → float64, float32 → float64). Everything else needs an explicit `as`, which keeps silent precision bugs out of your code.

### Control flow

```clear
if x > 10 and not done:
    ...
elseif x == 10:
    ...
else:
    ...

while i < n:
    i++

for i in 0..10:        // 0 to 9
for i in 1..=10:       // 1 to 10
for x in numbers:      // arrays, and any class with __len__ and __getitem__
    if x < 0:
        continue
    if x > 100:
        break

switch command:        // no fallthrough
    case 1, 2:
        start()
    case Command.Stop:
        stop()
    default:
        print("unknown")

let label = when total > 20 use "big" otherwise "small"

defer free(buffer)     // runs when the block exits, however it exits
```

### Functions

Functions can be used before the line that defines them. Calls are checked: argument count and types.

```clear
function area(shape: Shape, size: float64) -> float64:
    switch shape:
        case Shape.Circle:
            return PI * size * size
        default:
            return size * size

function max[T](a: T, b: T) -> T:       // generic, T is inferred at the call
    return when a > b use a otherwise b

max(3, 9)
max[float64](1, 2)
```

### Classes

```clear
class Account:
    owner: *int8
    balance: float64 = 0.0                   // fields can have defaults

    function __init__(self: *Account, owner: *int8):
        self.owner = owner

    function deposit(self: *Account, amount: float64):
        self.balance += amount

let account = Account("ada")                 // runs __init__ on a stack value
account.deposit(25.5)
print(account)                               // Account(owner=ada, balance=25.5)

let p = Point(3, 4)                          // no __init__: fields in order
let q = Point { 3 }                          // struct literal, missing fields use defaults
```

Operators are customised with Python's dunder methods: `__add__ __sub__ __mul__ __div__ __mod__ __eq__ __ne__ __lt__ __le__ __gt__ __ge__ __getitem__ __setitem__ __len__`. Classes can be generic: `class Box[T]`, used as `Box(7)` (inferred) or `Box[int64](7)`.

### Enums

```clear
enum Shape:
    Circle
    Square = 10

let s = Shape.Circle
print(s)                // Shape.Circle
```

Enums are their own types. They never mix with integers unless you use `as`.

### Printing

`print` takes any number of values of any type and separates them with spaces. It is built into the compiler and becomes a single `printf` call, so it costs no more than writing the format string yourself:

```clear
let scores: [3; int] = {7, 8, 9}
print("total:", 42, 2.5, true, scores, Point(1, 2))
// total: 42 2.5 true [7, 8, 9] Point(x=1, y=2)
```

### Modules and C

```clear
import "math"                 // the standard library
import "lib/shapes"           // a file next to this one (.cl is implied)
import "lib/shapes" as shapes // shapes.square(2)

declare printf(format: *int8, args: ...) -> int32   // any C function
```

### Standard library

| module | contents |
| --- | --- |
| `math` | `PI`, `TAU`, `E`, `sqrt`, `pow`, `sin` … (libm), generic `min`, `max`, `clamp`, `abs`, `sign`, `gcd`, `is_prime`, `lerp` |
| `memory` | `allocate[T](count)`, `reallocate`, `release`, `copy` |
| `list` | `List[T]`, a growable array: `push`, `pop`, `[]`, `for x in list`, `contains`, `free` |
| `string` | `length`, `equals`, `starts_with`, `contains`, `to_int`, `to_float` |

---

## Performance

Clear compiles through LLVM with the target's own cost model, so tight loops are vectorized and small functions are inlined. `Benchmarks/` holds programs written in both Clear and C. `python3 Scripts/bench.py build/clearc` builds both at `-O3` and compares them (lower is better):

| benchmark | Clear vs `clang -O3` |
| --- | --- |
| recursive fib(38) | ~1.0× |
| sieve of Eratosthenes, 50M | ~1.0× |
| 700×700 matrix multiply | ~1.0× |
| struct-heavy vector math | ~1.0× |

---

## Working on the compiler

```
Source/Lexing      tokens and indentation
Source/Parsing     Pratt parser producing the AST
Source/Sema        name resolution, type checking, generics, implicit conversions
Source/AST         AST nodes and LLVM code generation
Source/Compilation build pipeline, imports, optimization, linking
Scripts/Errors.toml every diagnostic message (turned into a header at build time)
Standard/          the standard library, written in Clear
Tests/             one .cl file per test, with the expected output inside it
```

Run the tests with `ctest` from the build directory (it runs the suite at `-O1` and `-O3`), or directly:

```
python3 Scripts/run_tests.py build/clearc Tests
CLEAR_TEST_FLAGS=-O3 python3 Scripts/run_tests.py build/clearc Tests
```

A test is any `.cl` file. Its expected stdout goes in a trailing comment block:

```clear
function main() -> int32:
    print(1 + 2)
    return 0

// expect:
// 3
```

Use `// expect-error` for programs that must not compile and `// expect-exit: N` for exit codes.

## Open source

Clear is open source under the Apache 2.0 license and welcomes contributions. See [docs/ROADMAP.md](docs/ROADMAP.md) for what is planned and how features are chosen.
