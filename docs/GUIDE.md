# The Clear Guide

This guide shows how to build Clear on your own machine, run programs, run the tests, and use every feature of the language.

Every feature has a complete, runnable program in [`examples/`](../examples). Each example ends with the output it must print, and the test suite checks all of them. If a snippet here differs from its example file, the example file is right.

```
clearc run examples/10_inheritance.cl
```

---

## Part 1: Setting up

### 1.1 Requirements

| tool | why |
| --- | --- |
| Clang 18+ with C++23 (`<print>`) | builds the compiler and links your programs |
| LLVM 18 development files | the code generator |
| CMake 3.20+ | the build |
| Python 3 | the test runner and diagnostics generator |
| git | fetching the compiler's own dependencies, and Clear packages |
| Internet access (first build only) | CMake downloads `fast_float` and `toml++` |

**Ubuntu 24.04 / Debian** (this is what the project is tested on):

```
sudo apt install clang-18 llvm-18-dev libstdc++-14-dev zlib1g-dev libzstd-dev \
                 libedit-dev libcurl4-openssl-dev cmake python3 git
```

**macOS** (with Homebrew; this platform is untested, so please report problems):

```
brew install llvm@18 cmake python git
export LLVM_PREFIX="$(brew --prefix llvm@18)"
```

Then use `-DLLVM_DIR=$LLVM_PREFIX/lib/cmake/llvm -DCMAKE_CXX_COMPILER=$LLVM_PREFIX/bin/clang++ -DCMAKE_C_COMPILER=$LLVM_PREFIX/bin/clang` in the configure step below, and put `$LLVM_PREFIX/bin` on your `PATH` so `clang` links your programs.

**Windows**: use WSL (Ubuntu) and follow the Ubuntu steps.

### 1.2 Getting the code

```
git clone https://github.com/clearlanguage/clear.git
cd clear
git checkout claude/laughing-goodall-fykc6e     # the branch with everything in this guide
```

### 1.3 Building

Clear has two build types. A **Debug** build makes the compiler easy to step through in a debugger. A **Release** build makes the compiler itself fast; your programs are equally fast with either.

```
# configure once
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release \
      -DCMAKE_CXX_COMPILER=clang++-18 -DCMAKE_C_COMPILER=clang-18 \
      -DLLVM_DIR=/usr/lib/llvm-18/lib/cmake/llvm

# build (re-run this after every change to the compiler)
cmake --build build -j
```

The compiler is now `build/clearc`. You can put it on your `PATH`:

```
export PATH="$PWD/build:$PATH"
```

`clearc` finds the standard library (`Standard/`) through the path stored when it was built. If you move the binary elsewhere, set `CLEAR_STANDARD_DIR=/path/to/clear/Standard`.

### 1.4 Your first program

```clear
// hello.cl
function main() -> int32:
    print("hello, clear")
    return 0
```

```
clearc run hello.cl              # compile and run (shorthand: clearc hello.cl)
clearc build hello.cl            # write ./hello
./hello
```

### 1.5 Command line reference

| command | what it does |
| --- | --- |
| `clearc run file.cl [-- args]` | compile for this CPU and run; arguments after `--` go to the program |
| `clearc build file.cl` | write an executable named after the file |
| `clearc build file.cl -o out` | choose the output path |
| `clearc new <dir>` | start a project (`clear.toml` + `main.cl`) |
| `clearc run <dir>` / `clearc build <dir>` | run or build a project (output in `<dir>/build/`) |
| `clearc add <name> --git <url> [--tag t \| --branch b \| --rev r]` | add a git dependency |
| `clearc add <name> --path <dir>` | add a local dependency |
| `clearc fetch` / `clearc update` | download dependencies / move them to newer versions |

| option | meaning |
| --- | --- |
| `-O0` `-O1` `-O2` `-O3` | optimization (default `-O1`; `-O3` is fastest) |
| `--checks` / `--no-checks` | run-time safety checks on/off (default: on for `-O0`/`-O1`, off for `-O2`/`-O3`) |
| `--native`, `--cpu=<name>` | use every instruction of this CPU (or the named one); `run` does this already |
| `--emit-ir` | also write the LLVM IR (`.ll`), to see exactly what your code became |
| `-v` | print progress |

---

## Part 2: Testing locally

### 2.1 Run everything

```
cd build && ctest --output-on-failure
```

This runs four suites:

1. `Tests/` at the default level.
2. `Tests/` again at `-O3`.
3. Every program in `examples/`.
4. The package manager end-to-end test, which uses local git repositories and needs no network.

### 2.2 Run the language tests directly

```
python3 Scripts/run_tests.py build/clearc Tests              # everything
python3 Scripts/run_tests.py build/clearc Tests classes      # only paths containing "classes"
python3 Scripts/run_tests.py build/clearc examples           # the examples from this guide
CLEAR_TEST_FLAGS=-O3 python3 Scripts/run_tests.py build/clearc Tests   # with extra compiler flags
CLEAR_TEST_FLAGS=-O0 python3 Scripts/run_tests.py build/clearc Tests
python3 Scripts/test_packages.py build/clearc                # package manager
```

Each test prints `PASS` or `FAIL`. A failure shows the expected and actual output, or the compiler's error.

### 2.3 Writing a test

A test is any `.cl` file under `Tests/`. Write what it must print in a comment block at the end:

```clear
function main() -> int32:
    print(1 + 2)
    return 0

// expect:
// 3
```

Other markers:

| marker | meaning |
| --- | --- |
| `// expect-error` | the program must **fail** to compile (for testing diagnostics) |
| `// expect-exit: N` | the exit code (a crash from a failed check is `-6`, i.e. SIGABRT) |
| `// flags: --checks` | extra compiler flags for this test |

Files inside a folder named `lib` are helper modules, not tests (see `Tests/modules/lib`).

The test runner gives programs an empty stdin. Tests run in a temporary directory, but imports are resolved relative to the test file.

### 2.4 Benchmarks

```
python3 Scripts/bench.py build/clearc Benchmarks 5     # 5 runs each, Clear vs the same program in C
```

---

## Part 3: The language, feature by feature

Clear reads like Python and runs like C. Blocks are indentation. Every construct compiles to the machine code you would expect, without a garbage collector, exceptions or hidden allocations.

### 3.1 Values and types · [`examples/02_values_and_types.cl`](../examples/02_values_and_types.cl)

```clear
let count = 7                  // int (int32), inferred
let ratio = 2.5                // float64
let small: uint8 = 200         // literals adapt to the declared type
let big: int64 = count         // widening is automatic
let back = big as int32        // narrowing needs `as`
let name: str = "clear"
let empty: int                 // starts at 0, never garbage
let grid: [LIMIT; int] = {1, 2}  // arrays: missing elements are 0
let p = &count                 // a pointer; *p reads/writes through it
const LIMIT = 4                // a compile-time constant
```

| types | |
| --- | --- |
| integers | `int8 int16 int32 int64`, `uint8 … uint64`, `int` = `int32`, `uint` = `uint32` |
| floats | `float32`, `float64`, `float` = `float64` |
| other | `bool`, `str`, `*T` (pointer), `[N; T]` (array), `(A, B)` (tuple), `?T` (optional), `function(A) -> R` |

Operators: `+ - * / % **`, comparisons, `and or not`, bitwise `& | ^ ~ << >>`, compound `+= -= …`, `++ --`, `in` / `not in`, `len(x)`.

Implicit conversions only happen when no information can be lost. Everything else needs `as`; this rule exists so that silent precision bugs can't happen.

### 3.2 Control flow · [`examples/03_control_flow.cl`](../examples/03_control_flow.cl)

```clear
if x > 10:
    ...
else if x == 10:               // `elseif` also works
    ...
else:
    ...

for i in 0..5:                 // 0 to 4
for i in 1..=3:                // 1 to 3
for v in values:               // arrays, List, Map, generators, your own classes
while n < 10:                  // with break / continue

switch x:                      // no fallthrough
    case 1, 2:
        ...
    default:
        ...

let label = when v > 3 use "large" otherwise "small"   // the conditional expression
assert total == 16, "total should be 16"
defer print("runs when the block exits, however it exits")
```

### 3.3 Functions · [`examples/04_functions.cl`](../examples/04_functions.cl)

```clear
function greet(name: str, greeting: str = "hello") -> str:   // default value
    return greeting

greet("ada")
greet("ada", greeting = "hi")       // keyword argument
describe(height = 2, width = 5)     // any order by name
```

A function can be called before the line that defines it. Calls are checked for argument count and types.

### 3.4 Tuples and unpacking · [`examples/05_tuples_and_unpacking.cl`](../examples/05_tuples_and_unpacking.cl)

```clear
function divmod(a: int, b: int) -> (int, int):
    return a / b, a % b

let q, r = divmod(17, 5)       // destructuring
a, b = b, a                    // swap
add3(values...)                // spread an array or tuple into the arguments
let t = (1, 2.5, "three")      // t[0], t[2]  (the index must be a constant)
```

### 3.5 Lambdas and function values · [`examples/06_lambdas.cl`](../examples/06_lambdas.cl)

```clear
function apply(f: function(int) -> int, x: int) -> int:
    return f(x)

apply(twice, 4)                       // a named function as a value
apply(lambda x: x + 100, 1)           // parameter types come from `apply`
let shifted = lambda (x: int): x + offset   // captures a *copy* of offset
```

A lambda that uses outside variables becomes a small object that holds copies of them. To pass such a lambda to your own function, give the parameter a generic type (`function run[F](f: F)`). A plain `function(...)` parameter only accepts lambdas that use no outside variables.

### 3.6 Classes · [`examples/07_classes.cl`](../examples/07_classes.cl)

```clear
class Account:
    owner: str
    balance: float64 = 0.0            // field default

    function init(self, owner: str):  // optional constructor
        self.owner = owner

    function deposit(self, amount: float64):
        self.balance += amount

let account = Account("ada")          // runs init
let p = Point(3, 4)                   // no init: fields in order
let q = Point(y = 1, x = 2)           // or by name
let r = Point { 5 }                   // struct literal; missing fields use defaults / zero
```

Methods take `self`, a pointer to the object, so they can change it. Writing `self: *Account` means the same thing. To work on a copy instead, write `self: Account`. Objects live on the stack unless you allocate them yourself.

### 3.7 Operator overloading · [`examples/08_operator_overloading.cl`](../examples/08_operator_overloading.cl)

Operators are methods written with `operator` instead of `function`:

```clear
class Vec2:
    x: float64
    y: float64

    operator add(self, other: Vec2) -> Vec2:
        return Vec2(self.x + other.x, self.y + other.y)

    operator str(self) -> str:
        return "<vector>"
```

| operator | enables |
| --- | --- |
| `add subtract multiply divide modulo power` | `a + b`, `a - b`, `a * b`, `a / b`, `a % b`, `a ** b` |
| `equals not_equals less less_equal greater greater_equal` | `==` `!=` `<` `<=` `>` `>=` (`!=` falls back to `not equals`) |
| `get` | `obj[i]`. If it returns a pointer (`-> *T`), `obj[i]` *is* the element: `obj[i] = v`, `obj[i] += 1` and `obj[i].field = v` change it in place |
| `set` | `obj[k] = v` when you need custom insertion (`Map` uses it to add new keys) |
| `len` | `len(obj)`; with `get`, also `for x in obj` |
| `iterate` | `for x in obj`, written as a generator: `operator iterate(self) -> Generator[T]` |
| `contains` | `x in obj` |
| `call` | `obj(args)` |
| `str` | what `print(obj)` shows |
| `hash` | `hash(obj)`, so it can be a `Map` key |
| `destruct` | cleanup when the object's scope ends (see automatic cleanup below) |
| `copy` | what `let b = a` makes when the object owns memory (see automatic cleanup below) |

Python-style names like `__add__` are a compile error that tells you the Clear spelling.

**Looping over objects.** `for item in cart` visits each object *in place*: `item.qty = 0` changes the item in the list or array. Numbers and other plain values are copied, as in Python. `let x = cart[0]` always makes a copy.

### 3.8 Generics · [`examples/09_generics.cl`](../examples/09_generics.cl)

```clear
function largest[T](a: T, b: T) -> T:
    return when a > b use a otherwise b

class Box[T]:
    value: T

largest(3, 9)            // T = int, inferred
largest[float64](1, 2)   // or given
let b = Box(42)          // Box[int]
let s = Box[str]("text")
```

Each set of type arguments produces its own specialised copy (like C++ templates, so there is no run-time cost). Methods of generic classes are only checked when they are used.

### 3.9 Inheritance · [`examples/10_inheritance.cl`](../examples/10_inheritance.cl)

```clear
class Animal:
    name: str

    function sound(self) -> str:
        return "..."

    function speak(self):
        print(self.name, "says", self.sound())

class Dog(Animal):                  // Dog has all of Animal's fields and methods
    function sound(self) -> str:    // replaces Animal's sound
        return "woof"

    function speak(self):
        super.speak()               // Animal's version

function introduce(a: *Animal):     // a *Dog converts to *Animal automatically
    a.speak()                       // prints "rex says woof" for a Dog
```

- A method call always runs **the object's own version**, even through a `*Animal`. There is no `virtual` keyword (writing it is an error that explains this).
- Only classes that inherit or are inherited from pay for this: they carry one hidden pointer to a method table. Every other class is laid out exactly as its fields.
- A class has one base class. Traits are listed in the same parentheses.

### 3.10 Properties · [`examples/11_properties.cl`](../examples/11_properties.cl)

```clear
property fahrenheit(self: *Temperature) -> float64:           // getter: t.fahrenheit
    return self.celsius * 9.0 / 5.0 + 32.0

property fahrenheit(self: *Temperature, value: float64):      // setter: t.fahrenheit = v
    self.celsius = (value - 32.0) * 5.0 / 9.0
```

`t.fahrenheit += 18.0` calls the getter and then the setter. A property without a setter can't be assigned to (compile error).

### 3.11 Traits · [`examples/12_traits.cl`](../examples/12_traits.cl)

```clear
trait Shape:
    function area(self: *Shape) -> float64

class Circle(Shape):                    // checked: Circle must have area(self) -> float64
    ...

function report[T: Shape](shape: *T):   // only types that satisfy Shape
    print(shape.area())
```

Traits work at compile time (static dispatch). For run-time polymorphism, use a base class.

### 3.12 Enums (with data) · [`examples/13_enums_and_variants.cl`](../examples/13_enums_and_variants.cl)

```clear
enum Color:                  // plain enum; never mixes with ints without `as`
    Red
    Blue = 10

enum Shape:                  // cases can carry data
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

let r = Shape.Rect(width = 4.0, height = 1.0)
r is Shape.Rect               // true
```

A `switch` over an enum must cover every case or have a `default`. Otherwise it's a compile error that names the missing cases.

### 3.13 Optionals and unions · [`examples/14_optionals_and_unions.cl`](../examples/14_optionals_and_unions.cl)

```clear
function find(values: [4; int], target: int) -> ?int:
    ...
    return none

let found = find(data, 9)
found.value                  // the int (checked: reading none stops the program)
found.value_or(-1)
found is none / found is not none
switch found:
    case some(index):
        ...
    case none:
        ...

union Bits:                  // fields share memory
    i: int64
    f: float64
let b = Bits(f = 1.0)
```

`none` can only go into a `?T`. Writing `let n: int = none` is a compile error.

### 3.14 Variants · [`examples/24_variants.cl`](../examples/24_variants.cl)

A `variant` holds a value of one of its types and remembers which one, like a union that knows what it holds:

```clear
variant Number:
    int
    float64

let n: Number = 2.5            // holds a float64
n is float64                   // true
n as float64                   // 2.5
n = 7                          // now holds an int
switch n:                      // every type must be handled
    case int(i):
        ...
    case float64(f):
        ...
```

`n as float64` when `n` holds an int stops the program with `panic: reading float64 from a Number that holds another type`. This check is always on, because the alternative would be reading garbage. A value that isn't one of the types (`let n: Number = "text"`) is a compile error. Plain `union` stays available when you want the raw shared bytes.

### 3.15 Generators · [`examples/15_generators.cl`](../examples/15_generators.cl)

```clear
function count_up(start: int, stop: int) -> Generator[int]:
    let i = start
    while i < stop:
        yield i
        i += 1

for i in count_up(3, 6):     // 3 4 5
    print(i)
```

- Values are produced only when the loop asks, so a generator can be endless (`fibonacci()` in the example).
- `break` cleans the generator up.
- By hand: `g.advance()` (true when a new value is ready), `g.value()`, `g.done()`, `g.free()`.

### 3.16 Async / await · [`examples/16_async_await.cl`](../examples/16_async_await.cl)

```clear
async function add(a: int, b: int) -> int:        // returns a Task[int]
    return a + b

async function worker(name: str, steps: int) -> int:
    for i in 0..steps:
        await pause()                             // give other tasks a turn
    return await add(steps, 100)                  // run another task, get its result

add(2, 3).run()                                   // from ordinary code: run to the end
```

There is no hidden event loop or thread. A task runs only when something resumes it:

- `task.run()` runs it to the end.
- `await other` (inside an async function) runs `other`. Each time `other` pauses, this task pauses too.
- `task.resume()`, `task.done()`, `task.result()` and `task.free()` let you write your own scheduler. The example interleaves two workers round-robin.

Generators and tasks are compiled to LLVM coroutines.

### 3.17 Macros · [`examples/17_macros.cl`](../examples/17_macros.cl)

```clear
macro square(x):                 // a single expression: usable as a value
    x * x

macro swap(a, b):                // statements: pasted where it's used
    let tmp = a
    a = b
    b = tmp

print(square!(7))
swap!(x, y)
```

- The `!` marks a macro use, so you can always tell a macro from a call.
- Arguments are pasted as written. That is why `swap!` can assign to `x` and `y`, but it also means an argument like `f()` runs once per use.
- Variables the macro declares get private names, so `tmp` above never clashes with a `tmp` of yours.
- Macros can be imported like functions.

### 3.18 Safety checks · [`examples/18_safety_checks.cl`](../examples/18_safety_checks.cl)

**Compile time** (always on; these mistakes are errors):

- a constant index out of range
- division by a constant zero
- dereferencing `null`
- a missing `return`
- a `switch` that misses enum cases
- lossy conversions without `as`
- `none` assigned to a non-optional
- `yield` or `await` in the wrong kind of function
- a class missing a method its trait requires

**Run time** (on for `-O0`/`-O1`; `--checks` forces them on, `--no-checks` off):

- array and `List`/`String`/`Map` bounds
- integer division by zero
- signed overflow
- null pointers
- reading `none`

A failed check stops the program with the source location:

```
panic: index out of range for an array of 3 (18_safety_checks.cl:15:18)
```

### 3.19 Strings · [`examples/19_strings.cl`](../examples/19_strings.cl)

There are two string types:

| | `str` | `String` (`import "string"`) |
| --- | --- | --- |
| what | a C string, the type of `"literals"` | an owned, growable string on the heap |
| cost | free | allocates; freed automatically at the end of its scope |
| `==`, `in`, `len`, print | yes | yes, plus `+`, `<`, `find`, `slice`, `strip`, `upper`, `lower`, `starts_with`, `ends_with`, `to_int`, `to_float`, `append`, `push`, `from_int` … |

### 3.20 Collections · [`examples/20_collections.cl`](../examples/20_collections.cl)

```clear
import "list"
import "map"

let numbers = List[int]()    // freed automatically at the end of the scope
numbers.push(4)              // also: pop, last, contains, clear, is_empty, numbers[i], len, for
numbers[0] += 1              // numbers[i] is the element itself

let ages = Map[str, int]()
ages["ada"] = 36             // also: m[k] += 1, get (-> ?V), get_or, `in`, remove, len, for key in m
```

Map keys can be numbers, enums, pointers, `str`, `String`, or any class with `operator hash` and `operator equals`.

### 3.21 Automatic cleanup · [`examples/25_automatic_cleanup.cl`](../examples/25_automatic_cleanup.cl)

Clear has no garbage collector and you never have to call `free()`. A value that owns memory is cleaned up when its scope ends:

- at the end of the block it was declared in, and on `return`, `break` and `continue`;
- for a parameter taken by value, when the function returns;
- for a value that is made and never stored (`make_list()` on its own line, `print(make_name())`), straight away or at the end of the block.

`String`, `List`, `Map` (and their elements) and `File` (it closes) already do this. A class gets it by defining `operator destruct`, or automatically when its fields need it:

```clear
class Connection:
    name: str

    operator destruct(self):
        print("closing", self.name)

class Session:                 // no destruct needed: its String and Connection are cleaned up
    user: String
    link: Connection
```

**Reading copies, writing goes in place.** Each value that owns memory has exactly one owner, so nothing is ever freed twice:

| you write | what happens |
| --- | --- |
| `let x = list[0]`, `let b = a`, `f(a)`, `list.push(a)`, `return self.name`, `let s = maybe.value` | **reading**: a separate copy with its own memory; the original is untouched |
| `list[0].append("!")`, `list[0] = s`, `list[0].qty += 1`, `for item in list`, `case some(s):` | **in place**: works on the element itself, no copy |
| `return s` (a local) | handed over without a copy (s ends here anyway) |
| `x = new_value` | the old value of `x` is cleaned up first |

Copies allocate, so in hot loops prefer working in place (`for w in words`, `words[i].method()`). A copy is made with `operator copy` if the class has one, otherwise field by field. `String`, `List` and `Map` copy their contents.

**Values that can't be copied.** A class with its own `operator destruct` and no `operator copy` (a `File`, a network connection) can't be duplicated safely. Assigning one *moves* it and leaves the old variable empty (all zero, so its cleanup does nothing), and copying one out of a field or list is a compile error. Give the class an `operator copy` if copying it makes sense.

`free()` is still there to give memory back early; the automatic cleanup afterwards does nothing. Code that manages raw memory itself (like `List`) uses `destroy(p)` to clean up `*p`, `take(p)` to hand a value out of raw memory without copying it, and `clone(p)` to copy it.

The cost is visible and predictable: a copy where you read an owning value, a cleanup call where a scope ends, and nothing running in the background.

### 3.22 Files and input · [`examples/21_files.cl`](../examples/21_files.cl)

```clear
import "io"

write_file(path, "text\n")           // -> bool
append_file(path, "more\n")
let text = read_file(path)           // -> ?String
let file = open(path, "r")           // -> ?File ; f.read_line() -> ?String, f.write(..), f.close()
let name = input("name? ")           // a line from the keyboard
file_exists(path) / delete_file(path)
```

### 3.23 Modules · [`examples/22_modules.cl`](../examples/22_modules.cl)

```clear
import "math"                    // standard library: Standard/math.cl
import "lib/geometry"            // examples/lib/geometry.cl (.cl implied)
import "lib/geometry" as geo     // geo.square(2)
```

A module is just a `.cl` file, and everything at its top level can be imported. Imports are looked up in this order:

1. next to the importing file
2. installed packages
3. the standard library

So a file of yours named `math.cl` hides the standard `math`, as in Python.

Standard library: `math`, `memory` (`allocate[T]`, `release`, …), `list`, `map`, `string`, `io`.

### 3.24 Calling C · [`examples/23_c_interop.cl`](../examples/23_c_interop.cl)

```clear
declare printf(format: *int8, args: ...) -> int32
declare abs(n: int32) -> int32
```

Any function from the C library can be declared and called directly. `*int8` is a C `char*`; `str` converts to it automatically.

---

## Part 4: Projects and packages

A project is a directory with a `clear.toml`:

```
clearc new myapp           # creates myapp/clear.toml, myapp/main.cl, myapp/.gitignore
clearc run myapp           # prints: hello from myapp
```

`clear.toml`:

```toml
[package]
name = "myapp"
version = "0.1.0"
main = "main.cl"           # the program
# lib = "myapp.cl"         # what other projects get from `import "myapp"`
#                          # (default: myapp.cl, else lib.cl, else main.cl)

[dependencies]
colors = { git = "https://github.com/someone/colors", tag = "v1.2" }   # or branch = "...", rev = "<commit>"
shapes = { path = "../shapes" }                                      # a local directory
```

Adding and using a dependency:

```
cd myapp
clearc add colors --git https://github.com/someone/colors --tag v1.2
clearc add shapes --path ../shapes
```

```clear
import "colors"            // the package's lib file
import "colors/extra"      // another file inside the package
```

What happens:

- `run`, `build` and `fetch` clone missing git dependencies into `myapp/.clear/packages/`. Dependencies of dependencies are fetched too.
- The exact commit of every git dependency is written to `clear.lock`. Commit `clear.lock`, so everyone (and CI) builds the same code. A fresh clone with `clearc fetch` gets exactly those commits back.
- `clearc update` moves dependencies to the newest commit their tag, branch or default branch allows, and rewrites `clear.lock`.
- A `path` dependency is relative to the `clear.toml` that declares it, and is used in place without copying.
- `clearc build myapp` writes `myapp/build/myapp`.

To try packages locally without publishing anything, make a git repo on disk and use a `file://` URL. `Scripts/test_packages.py` does exactly that.

---

## Part 5: Troubleshooting

| problem | fix |
| --- | --- |
| `Could not find a package configuration file provided by "LLVM"` | pass `-DLLVM_DIR=/usr/lib/llvm-18/lib/cmake/llvm` (or your LLVM's `lib/cmake/llvm`) |
| `fatal error: 'print' file not found` | the C++ library is too old: install `libstdc++-14-dev` (Ubuntu) or use Homebrew LLVM's clang (macOS) |
| CMake fails downloading `fast_float` / `tomlplusplus` | the first configure needs internet access |
| `clang: command not found` when building a Clear program | `clearc` links with `clang`; put it on your `PATH` (e.g. `sudo ln -s /usr/bin/clang-18 /usr/bin/clang`) |
| `import "math"` not found after moving `clearc` | `export CLEAR_STANDARD_DIR=/path/to/clear/Standard` |
| an import finds the wrong file | a file next to yours with the same name wins; rename one of them |
| `panic: ...` at run time | a safety check fired; the message gives file:line:column |
| a program is slower than expected | build with `-O3` (`-O1` is the default and keeps the safety checks) |
| `could not clone ...` | check the URL with `git ls-remote <url>`; private repos need git credentials configured |

---

## Part 6: Working on the compiler

```
Source/Lexing        tokens and indentation
Source/Parsing       Pratt parser producing the AST
Source/Sema          name resolution, type checking, generics, lowering (for loops, macros, lambdas…)
Source/AST           AST nodes and LLVM code generation (ASTNode.cpp; BuiltinPrint.cpp for print)
Source/Symbols       types (Type.h), the type registry, symbols
Source/Compilation   build pipeline, imports, optimization, linking
Source/Packages      clear.toml, dependencies, clear.lock
Scripts/Errors.toml  every diagnostic message (turned into a header at build time)
Standard/            the standard library, written in Clear
Tests/, examples/    the test suites
```

A typical change goes through these steps:

1. Parse the new syntax in `Parser.cpp`.
2. Check it, or turn it into existing nodes, in `Sema.cpp`.
3. Generate code in `ASTNode.cpp`.
4. Add a diagnostic to `Scripts/Errors.toml` if needed.
5. Add a test under `Tests/`.
6. Run `ctest`.

`clearc build file.cl --emit-ir` shows the LLVM IR, which is the quickest way to check what your change generates.
