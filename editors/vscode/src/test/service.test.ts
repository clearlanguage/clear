import { test } from "node:test";
import * as assert from "node:assert/strict";
import * as fs from "fs";
import * as os from "os";
import * as path from "path";
import { parse } from "../analyzer";
import { Project } from "../project";
import { LanguageService } from "../service";
import { parseDiagnostics } from "../diagnostics";

const REPO = path.resolve(__dirname, "..", "..", "..", "..");
const STANDARD = path.join(REPO, "Standard");

/** Writes files into a fresh folder and returns a service over them. `|` in a file marks the cursor. */
function setup(files: Record<string, string>) {
    const dir = fs.mkdtempSync(path.join(os.tmpdir(), "clear-vscode-"));
    const cursors: Record<string, { line: number; character: number }> = {};

    for (const [name, content] of Object.entries(files)) {
        const at = content.indexOf("|");
        let text = content;
        if (at >= 0 && name.endsWith(".cl")) {
            const before = content.slice(0, at);
            cursors[name] = { line: before.split("\n").length - 1, character: at - before.lastIndexOf("\n") - 1 };
            text = content.slice(0, at) + content.slice(at + 1);
        }
        fs.mkdirSync(path.dirname(path.join(dir, name)), { recursive: true });
        fs.writeFileSync(path.join(dir, name), text);
    }

    const project = new Project({ standardLibrary: () => STANDARD, openDocument: () => undefined });
    const service = new LanguageService(project, () => STANDARD);
    const file = (name: string) => path.join(dir, name);
    const cursor = (name: string) => cursors[name];
    return { dir, service, file, cursor };
}

const labels = (entries: { label: string }[]) => entries.map((e) => e.label);

const SHAPES = `import "math"

// A point on the plane.
class Point:
    x: float64
    y: float64 = 0.0   // defaults to the x axis

    function length(self) -> float64:
        return sqrt(self.x * self.x + self.y * self.y)

    operator add(self, other: Point) -> Point:
        return Point(self.x + other.x, self.y + other.y)

enum Color:
    Red
    Blue = 10

enum Shape:
    Circle(radius: float64)
    Rect(width: float64, height: float64)

// Adds two numbers.
function add(a: int, b: int = 1) -> int:
    return a + b

function largest[T](a: T, b: T) -> T:
    return when a > b use a otherwise b
`;

test("parse finds declarations, members, docs and imports", () => {
    const parsed = parse(SHAPES);
    assert.deepEqual(parsed.imports.map((i) => i.path), ["math"]);

    const point = parsed.symbols.find((s) => s.name === "Point")!;
    assert.equal(point.kind, "class");
    assert.equal(point.doc, "A point on the plane.");
    assert.deepEqual(point.children.map((c) => `${c.kind}:${c.name}`), ["field:x", "field:y", "method:length", "operator:add"]);
    assert.equal(point.children[1].doc, "defaults to the x axis");
    assert.equal(point.children[1].type, "float64");

    const add = parsed.symbols.find((s) => s.name === "add")!;
    assert.equal(add.doc, "Adds two numbers.");
    assert.deepEqual(add.params, [{ name: "a", type: "int", defaultValue: undefined }, { name: "b", type: "int", defaultValue: "1" }]);
    assert.equal(add.returnType, "int");

    const shape = parsed.symbols.find((s) => s.name === "Shape")!;
    assert.deepEqual(shape.children.map((c) => c.name), ["Circle", "Rect"]);
    assert.deepEqual(shape.children[1].params!.map((p) => p.name), ["width", "height"]);

    assert.deepEqual(parsed.symbols.find((s) => s.name === "largest")!.typeParams, ["T"]);
});

test("parse handles signatures over several lines, comments and strings with colons", () => {
    const parsed = parse(`function long(
    first: int,      // the first
    second: str = "a: b",
) -> int:
    let text = "class Fake:"
    /* function hidden(): */
    return first
`);
    assert.deepEqual(parsed.symbols.map((s) => s.name), ["long"]);
    assert.deepEqual(parsed.symbols[0].params!.map((p) => p.name), ["first", "second"]);
    assert.equal(parsed.symbols[0].params![1].defaultValue, "\"a: b\"");
    assert.deepEqual(parsed.locals.filter((l) => l.kind === "variable").map((l) => l.name), ["text"]);
});

test("completion after a dot lists fields and methods, also through lists and loops", () => {
    const { service, file, cursor } = setup({
        "shapes.cl": SHAPES,
        "main.cl": `import "shapes"

function main() -> int32:
    let points = List[Point]()
    for p in points:
        print(p.|)
    return 0
`,
    });
    const c = cursor("main.cl");
    const result = labels(service.completions(file("main.cl"), c.line, c.character));
    assert.deepEqual(result.sort(), ["length", "x", "y"]);
});

test("completion knows List's methods, indexing and method results", () => {
    const { service, file } = setup({
        "shapes.cl": SHAPES,
        "main.cl": `import "shapes"

function main() -> int32:
    let numbers = List[int]()
    let points = List[Point]()
    numbers.
    points[0].
    points.last().
    let name = String("ada")
    name.
    return 0
`,
    });
    const at = (line: number, character: number) => labels(service.completions(file("main.cl"), line, character));
    assert.ok(at(5, 12).includes("push"));
    assert.ok(at(5, 12).includes("sort_by"));
    assert.ok(!at(5, 12).includes("get"), "operators are not members");
    assert.deepEqual(at(6, 14).sort(), ["length", "x", "y"]);
    assert.deepEqual(at(7, 18).sort(), ["length", "x", "y"]);
    assert.ok(at(9, 9).includes("append"));
});

test("completion after a type name lists enum cases; after an alias, the module", () => {
    const { service, file } = setup({
        "lib/shapes.cl": SHAPES,
        "main.cl": `import "lib/shapes" as geo

function main() -> int32:
    let c = geo.
    let k = Color.
    return 0
`,
        "other.cl": `import "lib/shapes"

function main() -> int32:
    let k = Color.
    return 0
`,
    });
    const aliased = labels(service.completions(file("main.cl"), 3, 16));
    assert.ok(aliased.includes("Point") && aliased.includes("add") && aliased.includes("largest"));
    assert.deepEqual(labels(service.completions(file("other.cl"), 3, 18)).sort(), ["Blue", "Red"]);
});

test("completion in scope has locals, parameters, imports and auto-imports", () => {
    const { service, file } = setup({
        "main.cl": `function area(width: float64, height: float64) -> float64:
    let result = width * height

    return result
`,
    });
    const entries = service.completions(file("main.cl"), 2, 4);
    const names = labels(entries);
    for (const expected of ["width", "height", "result", "area", "print", "let", "int64", "String"]) assert.ok(names.includes(expected), expected);

    const sqrt = entries.find((e) => e.label === "sqrt")!;
    assert.equal(sqrt.addImport, "math");
    assert.ok(!names.includes("malloc"), "C plumbing is not suggested");
});

test("completion inside an import string lists modules and files", () => {
    const { service, file } = setup({
        "lib/geometry.cl": "function area() -> int:\n    return 1\n",
        "main.cl": `import "`,
    });
    const names = labels(service.completions(file("main.cl"), 0, 8));
    for (const expected of ["math", "list", "io", "lib/"]) assert.ok(names.includes(expected), expected);

    fs.writeFileSync(file("main.cl"), `import "lib/`);
    assert.deepEqual(labels(service.completions(file("main.cl"), 0, 12)), ["lib/geometry"]);
});

test("no completion inside comments or strings", () => {
    const { service, file } = setup({ "main.cl": `let s = "abc. "\n// p.\n` });
    assert.deepEqual(service.completions(file("main.cl"), 0, 13), []);
    assert.deepEqual(service.completions(file("main.cl"), 1, 5), []);
});

test("hover shows signatures, docs, inferred types and keyword help", () => {
    const { service, file } = setup({
        "shapes.cl": SHAPES,
        "main.cl": `import "shapes"

function main() -> int32:
    let total = add(1, 2)
    let p = Point(1.0, 2.0)
    let n = p.length()
    print(total, n)
    return 0
`,
    });
    const hover = (line: number, character: number) => service.hover(file("main.cl"), line, character);

    const call = hover(3, 17)!;
    assert.equal(call.code, "function add(a: int, b: int = 1) -> int");
    assert.match(call.doc!, /Adds two numbers/);
    assert.match(call.doc!, /shapes\.cl/);

    assert.equal(hover(3, 9)!.code, "let total: int");
    assert.equal(hover(4, 9)!.code, "let p: Point");
    assert.equal(hover(5, 9)!.code, "let n: float64");
    assert.equal(hover(5, 15)!.code, "function length(self) -> float64");
    assert.equal(hover(6, 6)!.code, "print(values...)");
    assert.match(hover(2, 2)!.doc!, /Declares a function/);
    assert.match(hover(0, 10)!.code!, /import "shapes"/);
});

test("go to definition reaches other files and the standard library", () => {
    const { service, file } = setup({
        "shapes.cl": SHAPES,
        "main.cl": `import "shapes"

function main() -> int32:
    let numbers = List[int]()
    numbers.push(add(1))
    let s = Shape.Rect(1.0, 2.0)
    return 0
`,
    });
    const push = service.definition(file("main.cl"), 4, 14)!;
    assert.equal(push.file, path.join(STANDARD, "list.cl"));
    assert.match(fs.readFileSync(push.file, "utf8").split("\n")[push.line], /function push/);

    const add = service.definition(file("main.cl"), 4, 18)!;
    assert.equal(add.file, file("shapes.cl"));
    assert.equal(add.line, 22);

    const rect = service.definition(file("main.cl"), 5, 19)!;
    assert.equal(rect.line, 19);

    const importTarget = service.definition(file("main.cl"), 0, 10)!;
    assert.equal(importTarget.file, file("shapes.cl"));

    const local = service.definition(file("main.cl"), 4, 6)!;
    assert.deepEqual([local.line, local.character], [3, 8]);
});

test("signature help for functions, methods, constructors and builtins", () => {
    const { service, file } = setup({
        "shapes.cl": SHAPES,
        "main.cl": `import "shapes"

function main() -> int32:
    add(1,
    let p = Point(
    let numbers = List[int]()
    numbers.push(
    print(
    return 0
`,
    });
    const sig = (line: number, character: number) => service.signature(file("main.cl"), line, character);

    assert.deepEqual(sig(3, 11), { label: "add(a: int, b: int = 1) -> int", params: ["a: int", "b: int = 1"], activeParameter: 1, doc: "Adds two numbers." });
    assert.deepEqual(sig(4, 18)!.params, ["x: float64", "y: float64 = 0.0"]);
    assert.deepEqual(sig(6, 17)!.params, ["value: int"]);
    assert.equal(sig(7, 10)!.label, "print(values...)");
});

test("references follow locals in their scope and members through dots", () => {
    const { service, file } = setup({
        "shapes.cl": SHAPES,
        "main.cl": `import "shapes"

function main() -> int32:
    let x = 1
    let p = Point(1.0, 2.0)
    print(x, p.x, p.length())
    return x

function other(x: int) -> int:
    return x
`,
    });
    const files = [file("main.cl"), file("shapes.cl")];

    const local = service.references(file("main.cl"), 3, 8, files);
    assert.deepEqual(local.map((o) => [o.line, o.character]), [[3, 8], [5, 10], [6, 11]]);

    const field = service.references(file("main.cl"), 5, 15, files);
    assert.deepEqual(field.map((o) => `${path.basename(o.file)}:${o.line}`).sort(), ["main.cl:5", "shapes.cl:11", "shapes.cl:11", "shapes.cl:4", "shapes.cl:8", "shapes.cl:8"]);

    assert.equal(service.canRename(file("main.cl"), 5, 21).constructor, Object);
    assert.equal(typeof service.canRename(file("main.cl"), 4, 0), "string");
});

test("inferred types for let without a written type", () => {
    const { service, file } = setup({
        "main.cl": `function main() -> int32:
    let a = 1
    let b = 2.5
    let c = "x" + "y"
    let d = a > 1
    let e: int64 = 4
    let numbers = List[int]()
    let first = numbers[0]
    let point = Point { 5 }
    return 0

class Point:
    x: int
`,
    });
    const hints = service.inferredTypes(file("main.cl"), 0, 100).map((h) => `${h.line}:${h.type}`);
    assert.deepEqual(hints, ["1:int", "2:float64", "3:String", "4:bool", "7:int"]);
});

test("modulesDeclaring finds the import that defines a name", () => {
    const { service } = setup({});
    assert.deepEqual(service.modulesDeclaring("sqrt"), ["math"]);
    assert.deepEqual(service.modulesDeclaring("read_file"), ["io"]);
});

test("clearc check output becomes diagnostics", () => {
    const output = `error[E048]: Cannot convert implicitly, the conversion may lose information.
  --> /tmp/bad.cl:2:18
  |
2 |     let x: int = "hi"
  |                  ^^^^
  = help: Converting 'str' to 'int32' needs an explicit cast.

warning[E091]: Pointer into a changed collection.
  --> /tmp/bad.cl:10:5
   |
10 |     print(*p)
   |     ^
`;
    assert.deepEqual(parseDiagnostics(output), [
        { severity: "error", code: "E048", message: "Cannot convert implicitly, the conversion may lose information.", file: "/tmp/bad.cl", line: 1, character: 17, length: 4, help: "Converting 'str' to 'int32' needs an explicit cast." },
        { severity: "warning", code: "E091", message: "Pointer into a changed collection.", file: "/tmp/bad.cl", line: 9, character: 4, length: 1 },
    ]);
});

test("every Clear file in the repository parses, and every example has main", () => {
    const files: string[] = [];
    const walk = (dir: string) => {
        for (const entry of fs.readdirSync(dir, { withFileTypes: true })) {
            if (entry.name.startsWith(".") || entry.name === "node_modules" || entry.name === "build" || entry.name === "editors") continue;
            const full = path.join(dir, entry.name);
            if (entry.isDirectory()) walk(full);
            else if (entry.name.endsWith(".cl")) files.push(full);
        }
    };
    walk(REPO);
    assert.ok(files.length > 100);

    for (const f of files) {
        const parsed = parse(fs.readFileSync(f, "utf8"));
        if (f.includes(`${path.sep}examples${path.sep}`) && !f.includes(`${path.sep}lib${path.sep}`))
            assert.ok(parsed.symbols.some((s) => s.name === "main"), `${f} has main`);
    }
});
