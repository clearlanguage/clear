
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

**New here? Read [docs/GUIDE.md](docs/GUIDE.md)**: it covers setting up, testing, and every feature, with a runnable program for each in [`examples/`](examples).

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
clearc new app && clearc run app # a project with dependencies (see "Modules, packages and C")
```

| option | meaning |
| --- | --- |
| `-O0` … `-O3` | optimization level (default `-O1`; `-O3` is fastest) |
| `--native`, `--cpu=<name>` | use every instruction this CPU (or the named one) supports; `run` does this by default |
| `--emit-ir` | also write the LLVM IR as a `.ll` file |
| `-o <path>` | output path |
| `--checks`, `--no-checks` | run-time safety checks (default: on below `-O2`) |
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
let buffer: [LIMIT; int] = {}  // fixed size arrays (sizes may use consts), all zero
let p: *int = &count           // pointers, *p to dereference
let name: str = "clear"        // string literals; == compares their contents
let t = (1, 2.5, "three")      // tuples: t[0], t[2]
let empty: int                 // never garbage: starts at zero
```

Built-in types: `int8 … int64`, `uint8 … uint64`, `int` (= `int32`), `uint` (= `uint32`), `float32`, `float64`, `float` (= `float64`), `bool`, `str`, pointers `*T`, arrays `[N; T]`, tuples `(A, B)`, optionals `?T`, function types `function(int) -> int`.

Conversions that cannot lose information happen automatically (int → wider int, int → float64, float32 → float64, `*Derived` → `*Base`). Everything else needs an explicit `as`.

### Control flow

```clear
if x > 10 and not done:
    ...
else if x == 10:               // or elseif
    ...
else:
    ...

while i < n:
    i++

for i in 0..10:                // 0 to 9;  1..=10 includes 10
for x in numbers:              // arrays, List, Map, generators, any class with __len__ and __getitem__
    if x < 0:
        continue

switch command:                // no fallthrough; must cover every case of an enum (or have default)
    case 1, 2:
        start()
    default:
        print("unknown")

let label = when total > 20 use "big" otherwise "small"
defer free(buffer)             // runs when the block exits, however it exits
assert count > 0, "count must be positive"
print(3 in values, "ell" in "hello", len(values))
```

### Functions

Functions can be used before the line that defines them. Calls are checked: argument count and types.

```clear
function greet(name: str, greeting: str = "hello") -> str:   // default values
    return greeting

greet("ada", greeting = "hi")                                // keyword arguments

function divmod(a: int, b: int) -> (int, int):               // several results
    return a / b, a % b

let q, r = divmod(17, 5)                                     // destructuring
a, b = b, a                                                  // swap
add3(values...)                                              // spread an array or tuple into arguments

function max[T](a: T, b: T) -> T:                            // generic, T is inferred
    return when a > b use a otherwise b
```

Functions are values. Lambdas take their parameter types from where they are used. A lambda that uses outside variables keeps its own copies of them, made when the lambda is created:

```clear
function apply(f: function(int) -> int, x: int) -> int:
    return f(x)

apply(lambda x: x + 100, 1)
let offset = 10
let shifted = lambda (x: int): x + offset
```

### Classes

```clear
class Account:
    owner: str
    balance: float64 = 0.0                   // fields can have defaults

    function __init__(self: *Account, owner: str):
        self.owner = owner

    function deposit(self: *Account, amount: float64):
        self.balance += amount

let account = Account("ada")                 // runs __init__ on a stack value
let p = Point(3, 4)                          // no __init__: fields in order (or Point(y = 4, x = 3))
```

Operators are customised with Python's dunder methods: `__add__ __sub__ __mul__ __div__ __mod__ __pow__ __eq__ __ne__ __lt__ __le__ __gt__ __ge__ __getitem__ __setitem__ __len__ __contains__ __call__ __str__ __hash__`. Classes can be generic: `class Box[T]`, used as `Box(7)` or `Box[int64](7)`.

**Inheritance** puts the base's fields first, so a `*Dog` can be passed wherever a `*Animal` is expected. Calls are static (no hidden cost) unless a method is marked `virtual`, which adds one table pointer to the object:

```clear
class Animal:
    name: str

    virtual function sound(self: *Animal) -> str:
        return "..."

    function speak(self: *Animal):
        print(self.name, "says", self.sound())   // calls the object's own sound()

class Dog(Animal):
    function sound(self: *Dog) -> str:
        return "woof"

    function speak(self: *Dog):
        super.speak()                            // the base version
```

**Properties** look like fields but run code:

```clear
class Temperature:
    celsius: float64

    property fahrenheit(self: *Temperature) -> float64:
        return self.celsius * 9.0 / 5.0 + 32.0

    property fahrenheit(self: *Temperature, value: float64):
        self.celsius = (value - 32.0) * 5.0 / 9.0

t.fahrenheit = 212.0
```

**Traits** list methods a class promises to have. They are checked when the class is declared, and generic functions can require them; the call is resolved at compile time, so it costs nothing:

```clear
trait Shape:
    function area(self: *Shape) -> float64

class Circle(Shape):
    radius: float64

    function area(self: *Circle) -> float64:
        return 3.14159 * self.radius * self.radius

function total_area[T: Shape](shapes: *[4; T]) -> float64:
    ...
```

### Enums, variants, optionals and unions

```clear
enum Color:                       // plain enums never mix with integers without `as`
    Red
    Green = 10

enum Shape:                       // cases can carry data (tagged unions)
    Circle(radius: float64)
    Rect(width: float64, height: float64)
    Empty

    function area(self: *Shape) -> float64:
        switch *self:
            case Circle(r):
                return PI * r * r
            case Rect(w, h):
                return w * h
            case Empty:
                return 0.0

function find(values: [4; int], target: int) -> ?int:     // an int, or none
    ...
    return none

let found = find(data, 9)
if found is not none:
    print(found.value)
print(found.value_or(-1))

union Bits:                       // every field shares the same bytes
    i: int64
    f: float64
```

### Generators and async

A function that returns `Generator[T]` produces values with `yield`; nothing runs until the loop asks for the next one:

```clear
function fibonacci() -> Generator[int64]:
    let a: int64 = 0
    let b: int64 = 1
    while true:
        yield a
        a, b = b, a + b

for f in fibonacci():
    if f > 100:
        break
    print(f)
```

`async function` returns a `Task`. `await` runs another task, and when that task pauses, this one pauses too, so tasks can be interleaved by whoever runs them. There is no hidden scheduler or thread: `task.run()` runs a task to the end, and `resume()` / `done()` / `result()` let you write your own scheduler (see `Tests/async/tasks.cl`).

```clear
async function add(a: int, b: int) -> int:
    return a + b

async function worker(steps: int) -> int:
    for i in 0..steps:
        await pause()                  // let other tasks run
    return await add(steps, 1)

print(worker(3).run())                 // 4
```

Both compile to LLVM coroutines. When a generator or task does not outlive its caller, LLVM keeps its state on the stack.

### Macros

A macro pastes code in, with its arguments, where you write `name!(...)`. The `!` shows that it is a macro and not a call. Variables declared inside a macro can never clash with yours:

```clear
macro swap(a, b):
    let tmp = a
    a = b
    b = tmp

macro square(x):
    x * x

swap!(x, y)
print(square!(7))
```

### Printing

`print` takes any number of values of any type. It becomes a single `printf` call, so it costs no more than writing the format string yourself. A class can choose how it is printed with `__str__`.

```clear
print("total:", 42, 2.5, true, scores, Point(1, 2), (1, "a"), Shape.Circle(1.0))
// total: 42 2.5 true [7, 8, 9] Point(x=1, y=2) (1, a) Shape.Circle(radius=1.0)
```

### Safety checks

Mistakes that can be found at compile time are errors: a constant index out of range, a division by a constant zero, dereferencing `null`, a missing `return`, or a `switch` that misses a case. At run time, debug builds (`-O0`, `-O1`) check array bounds, integer division by zero, signed overflow, null pointers and unwrapping `none`, and stop with a message such as `panic: index out of range for an array of 3 (main.cl:5:13)`. Release builds (`-O2`, `-O3`) leave these checks out. Use `--checks` or `--no-checks` to choose either way.

### Modules, packages and C

```clear
import "math"                 // the standard library
import "lib/shapes"           // a file next to this one (.cl is implied)
import "lib/shapes" as shapes // shapes.square(2)
import "colors"               // a package from clear.toml

declare printf(format: *int8, args: ...) -> int32   // any C function
```

A project is a directory with a `clear.toml`:

```toml
[package]
name = "app"
main = "main.cl"

[dependencies]
colors = { git = "https://github.com/someone/colors", tag = "v1.2" }
shapes = { path = "../shapes" }
```

```
clearc new app                                         # clear.toml + main.cl
clearc add colors --git <url> --tag v1.2               # or --branch, --rev, --path <dir>
clearc run app                                         # fetches what is missing, builds, runs
clearc fetch                                           # download the exact versions in clear.lock
clearc update                                          # move to the newest versions the manifest allows
```

Dependencies are cloned into `.clear/packages`, dependencies of dependencies are followed, and the exact commits are recorded in `clear.lock`, so a fresh checkout builds the same code.

### Standard library

| module | contents |
| --- | --- |
| `math` | `PI`, `TAU`, `E`, `sqrt`, `pow`, `sin` … (libm), generic `min`, `max`, `clamp`, `abs`, `sign`, `gcd`, `is_prime`, `lerp` |
| `memory` | `allocate[T](count)`, `reallocate`, `release`, `copy` |
| `list` | `List[T]`, a growable array: `push`, `pop`, `[]`, `for x in list`, `contains`, `free` |
| `map` | `Map[K, V]`, a hash table: `m[k] = v`, `m[k]`, `get` (→ `?V`), `get_or`, `k in m`, `remove`, `for key in m`, `free` |
| `string` | `String`, an owned growable string (`append`, `+`, `==`, `<`, `find`, `slice`, `strip`, `upper`, `to_int`, `from_int` …), and helpers for `str` |
| `io` | `File`, `open`, `read_file`, `write_file`, `append_file`, `read_line`, `input`, `file_exists`, `delete_file` |

Nothing allocates behind your back: `List`, `Map`, `String` and file contents live on the heap because you created them, and `free()` gives the memory back.

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
