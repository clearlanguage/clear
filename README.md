---

# Clear Programming Language

Clear is a modern **compiled programming language** that combines the **simplicity of Python** with the **performance and control of low-level languages**. Designed with clarity, safety, and flexibility in mind, Clear is ideal for everything from systems programming to scripting — without sacrificing readability.

---

## 🚀 Why Clear?

* **Simple, readable syntax** inspired by Python
* **Compiled performance** approaching C/C++
* **Hybrid memory management** model
* **Powerful type system** with optional inference
* **Support for traits, structs, variadic functions, and more**
* **Modules and header file imports**
* **Built-in string interpolation, modern loops, and more**

---

## 📦 Example

```clear
import "io.cl"

function main() -> int:
    print("Hello, world!")
    return 0
```

---

## 🧠 Key Language Features

### ✅ Type Inference and Declarations

```clear
let x = 10          // inferred as int
let y = 'd'         // inferred as char
const int a = 5     // constant with explicit type
const b = 3.14      // constant with inferred type
```

### 🔁 Control Flow

```clear
if (x == 10 and y == 5) or not c:
    print("Matched")
elif x == 200:
    print("Another match")
else:
    print("Fallback")
```

### 🧱 Classes, Traits, Structs

```clear
class Car inherits Vehicle:
    int RegistrationNumber

    function hash():
        Object.hash(this.color + this.model)

    function __construct__(int x):
        this.RegistrationNumber = x
```

```clear
trait Shape:
    function area() -> float
    function draw()
    string fillColor
```

```clear
struct Hello:
    int data = 10
    string test
    Hello* next
```

### 🧮 Arrays

```clear
int[...] o = {1,2,3,4}   // infer array size
int[5] o = {1,2,3,4}     // fixed-size array
int[] o = {1,2,3,4}      // dynamic array

print(...o)             // unpack with variadic
```

### 📦 Variadic Functions

```clear
function print(args...):
    for i in args:
        _print(i)
```

### 🧵 String Interpolation

```clear
string x = `hello {user.name}`
```

### 📚 Modules and Imports

```clear
import "main.cl"
import "math.cl"
import "test/test.cl"
import "math.h"    // C-style header
```

---

## ⚙️ How to Run

To run a Clear program:

1. Compile the `.cl` file using the Clear compiler.
2. Execute the resulting binary.

```bash
clearc main.cl -o main
./main
```

> *Note: Replace `clearc` with the name of your compiler binary.*

---

## 🧹 Memory Management

Clear uses a **hybrid memory model**, combining automatic memory management with the ability to manually manage memory when performance or control is needed.

---

## 🌍 Open Source

Clear is open source and welcomes contributions!
---

## 🛣 Roadmap (Planned Features)

* [ ] Operator overloading
* [ ] Properties
* [ ] Async/await
* [ ] Module/package manager
* [ ] Lambdas

---
