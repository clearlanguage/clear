---

# Clear Programming Language

Clear is a modern **compiled programming language** that combines the **simplicity of Python** with the **performance and control of low-level languages**. Designed with clarity, safety, and flexibility in mind, Clear is ideal for everything from systems programming to scripting — without sacrificing readability.

---

## 📦 Build Instructions 

### Requirements 
* Clang compiler 
* CMake 
* LLVM built and cmake installed. See https://llvm.org/docs/CMake.html
* Currently only tested on linux and macos


### Building

* First clone the repository

```
git clone https://github.com/clearlanguage/clear.git
```

* Create a build directory 

```
mkdir build
cd build
```

* Use IDE CMake integration or call cmake directly 

```
cmake path/to/clear
cmake --build .
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

## Roadmap

### Completed Features 
* Variables 
```
int a = 5
float y = 2.0
char const* name = "alice"
```
* Functions 
```
function add(int a, int b) -> int:
    return a + b

function main():
    int result = add(5, 10)
```
* Operators
```
int sum = 5 + 3
int product = 4 * 2
bool isEqual = (sum == product)
```
* Pointers/Dereferencing 
```
int x = 42;
int* ptr = &x;   // pointer to x
int y = *ptr;    // dereference ptr
```
* Arrays 
```
int[5] nums = {1, 2, 3, 4, 5}
int first = nums[0]
```
* If/ElseIf/Else 
```
if x > 10:
    print("Greater than 10")
elseif x == 10:
    print("Exactly 10")
else:
    print("Less than 10");
```
* While loops
```
int i = 0;
while i < 5:
    print(i)
    i++
```
* Type Inference 
```
let i = 0 // int
const number = 2.0 // float const
```
* Structs 
```
struct Point:
    float x, float y
    float z

Point p = { 1.0, 2.0 }
```
* Basic Classes 
```
class Person:
    string name

    function greet():
        print("Hello ", this.name)


Person p = { "Bob" }
p.greet() // outputs "Hello bob"
```

* Traits 
```
trait Drawable:
    function draw()

class Circle:
    function draw():
        print("Drawing Circle")


function draw_object(Drawable drawable):
    drawable.draw()

Circle c
draw_object(c)

```

* Function Overloading
```
function _print(int64 a):
    printf("%i", a)

function _print(float64 a):
    printf("%d", a)
```
* Variadic Arguments 
```
function println(args...):
    for arg in args:
        _print(arg)
    
    printf("\n")
```
* Defer 
```
function test():
    defer print("Cleaning up!")
    print("Doing work...")
    // "Cleaning up!" will print when leaving this scope

```
* Named Blocks
```
outer:
    defer print("i am leaving outer block")
    print("Inside outer block")

    inner:
        defer print("i am leaving inner block")
        print("Inside inner block")

        // outputs i am leaving inner block when inner scope is finished
    
    // outputs i am leaving outer block when outer scope is finished
```
* Switch Statements
```
int a = ...
switch a:
    case 20, 30, 40:
        print("a was either 20, 30, or 40")
    default:
        print("not a valid input")
```
* Basic Enums 
```
enum Direction:
    Left, Right, Up, Down 

print(Direction.Left)   // outputs 0
print(Direction.Right)  // outputs 1
print(Direction.Up)     // outputs 2

```

### Planned Features

* Unions
* Variants
* Generics 
* Modules
* Inheritence 
* More Developed Enums
* Unpack Operator 
* Operator overloading
* Properties
* Async/await
* Package manager
* Lambdas
* Runtime and compile time saftey checks when indexing, derefencing and more
---

