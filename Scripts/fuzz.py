#!/usr/bin/env python3
"""Clear fuzzer: random valid programs that mix language features.

Every program is valid by construction, so any of these is a bug:

    crash      the compiler died (segfault, failed internal check)
    rejected   the compiler reported an error for a valid program
    runtime    the program crashed or exited with a non-zero code
    mismatch   the program printed something different at -O0 and -O3

Failing programs are kept in the output folder with what went wrong.

usage: fuzz.py <path to clearc> [--count N] [--seed S] [--out DIR] [--keep]
"""

import argparse
import os
import random
import shutil
import subprocess
import sys
import tempfile


class Program:
    """Builds one program. Values stay small (everything is reduced % 1000) so nothing overflows."""

    def __init__(self, rng):
        self.rng = rng
        self.lines = []
        self.imports = set()
        self.classes = []      # (name, has_destruct)
        self.functions = []    # names of int functions taking (int, int)
        self.generators = []
        self.enums = []        # (name, [case names])
        self.counter = 0

    def fresh(self, prefix):
        self.counter += 1
        return f"{prefix}{self.counter}"

    # ---------------------------------------------------------------- expressions

    def int_expr(self, scope, depth=0):
        r = self.rng
        choices = ["literal", "var"]
        if depth < 3:
            choices += ["binary", "binary", "call", "when"]
            if scope.lists:
                choices.append("element")
            if scope.objects:
                choices.append("method")
            if scope.lambdas:
                choices.append("lambda")
        kind = r.choice(choices)

        if kind == "var" and not scope.ints:
            kind = "literal"
        if kind == "call" and not self.functions:
            kind = "literal"

        if kind == "literal":
            return str(r.randint(0, 99))
        if kind == "var":
            return r.choice(scope.ints)
        if kind == "binary":
            op = r.choice(["+", "-", "*", "%"])
            left, right = self.int_expr(scope, depth + 1), self.int_expr(scope, depth + 1)
            if op == "%":
                return f"(({left}) % (({right}) % 7 + 8))"
            return f"((({left}) {op} ({right})) % 1000)"
        if kind == "call":
            return f"{r.choice(self.functions)}({self.int_expr(scope, depth + 1)}, {self.int_expr(scope, depth + 1)})"
        if kind == "when":
            return f"(when {self.cond(scope, depth + 1)} use {self.int_expr(scope, depth + 1)} otherwise {self.int_expr(scope, depth + 1)})"
        if kind == "element":
            xs = r.choice(scope.lists)
            # % keeps the sign (like C), so make the index positive first
            return f"{xs}[(({self.int_expr(scope, depth + 1)}) % len({xs}) + len({xs})) % len({xs})]"
        if kind == "method":
            return f"{r.choice(scope.objects)}.score({self.int_expr(scope, depth + 1)})"
        if kind == "lambda":
            name, arity = r.choice(scope.lambdas)
            args = ", ".join(self.int_expr(scope, depth + 1) for _ in range(arity))
            return f"(({name}({args})) % 1000)"
        raise AssertionError(kind)

    def cond(self, scope, depth=0):
        op = self.rng.choice(["<", "<=", ">", ">=", "==", "!="])
        return f"{self.int_expr(scope, depth + 1)} {op} {self.int_expr(scope, depth + 1)}"

    # ---------------------------------------------------------------- top level

    def add_function(self):
        name = self.fresh("f")
        scope = Scope(ints=["a", "b"])
        body = self.int_expr(scope)
        self.lines += [f"function {name}(a: int, b: int) -> int:", f"    return {body}", ""]
        self.functions.append(name)

    def add_class(self):
        name = self.fresh("C")
        destruct = self.rng.random() < 0.6
        self.imports.add("string")
        lines = [f"class {name}:", "    n: int", "    label: String", "",
                 "    function score(self, k: int) -> int:",
                 f"        return (self.n * 3 + k + len(self.label) as int) % 1000", "",
                 "    function bump(self):", "        self.n = (self.n + 1) % 1000", "        self.label.append(\"+\")", ""]
        if destruct:
            lines += ["    operator destruct(self):", f"        print(\"drop {name}\", self.n, self.label)", ""]
        self.lines += lines
        self.classes.append((name, destruct))

    def add_generator(self):
        name = self.fresh("g")
        lines = [f"function {name}(limit: int) -> Generator[int]:"]
        if self.classes and self.rng.random() < 0.5:
            cls = self.rng.choice(self.classes)[0]
            lines.append(f"    let keep = {cls}(1, String(\"{name}\"))")   # held while suspended
        lines += ["    let i = 0", "    while i < limit:", "        yield i * 2", "        i += 1", ""]
        self.lines += lines
        self.generators.append(name)

    def add_enum(self):
        name = self.fresh("E")
        cases = [self.fresh("K") for _ in range(self.rng.randint(2, 4))]
        self.lines += [f"enum {name}:"] + [f"    {case}(v: int)" for case in cases] + [""]
        self.enums.append((name, cases))

    # ---------------------------------------------------------------- statements

    def block(self, scope, indent, depth, count):
        for _ in range(count):
            self.statement(scope, indent, depth)

    def emit(self, indent, text):
        self.lines.append("    " * indent + text)

    def statement(self, scope, indent, depth):
        r = self.rng
        kinds = ["let", "let", "assign", "print", "print"]
        if depth < 3:
            kinds += ["if", "for", "while"]
        if self.classes:
            kinds += ["object", "object_use"]
        kinds += ["list", "list_use", "string"]
        if self.generators and depth < 3:
            kinds.append("generator")
        if depth < 2:
            kinds.append("lambda")
        if self.enums:
            kinds.append("enum")
        kinds.append("optional")
        kind = r.choice(kinds)

        if kind == "let":
            name = self.fresh("x")
            self.emit(indent, f"let {name} = {self.int_expr(scope)}")
            scope.ints.append(name)
        elif kind == "assign" and scope.mutable:
            target = r.choice(scope.mutable)
            op = r.choice(["=", "+="])
            self.emit(indent, f"{target} {op} {self.int_expr(scope)}")
            self.emit(indent, f"{target} = {target} % 1000")
        elif kind == "print":
            values = ", ".join(self.int_expr(scope) for _ in range(r.randint(1, 3)))
            self.emit(indent, f"print({values})")
        elif kind == "if":
            self.emit(indent, f"if {self.cond(scope)}:")
            self.block(scope.child(), indent + 1, depth + 1, r.randint(1, 3))
            if r.random() < 0.5:
                self.emit(indent, "else:")
                self.block(scope.child(), indent + 1, depth + 1, r.randint(1, 2))
        elif kind == "for":
            i = self.fresh("i")
            self.emit(indent, f"for {i} in 0..{r.randint(1, 5)}:")
            inner = scope.child(loop=True)
            inner.ints.append(i)
            self.block(inner, indent + 1, depth + 1, r.randint(1, 3))
            if r.random() < 0.4:
                self.emit(indent + 1, f"if {self.cond(inner)}:")
                self.emit(indent + 2, r.choice(["break", "continue"]))
        elif kind == "while":
            counter = self.fresh("w")
            self.emit(indent, f"let {counter} = 0")
            self.emit(indent, f"while {counter} < {r.randint(1, 4)}:")
            self.emit(indent + 1, f"{counter} += 1")
            inner = scope.child(loop=True)
            inner.ints.append(counter)
            self.block(inner, indent + 1, depth + 1, r.randint(1, 2))
        elif kind == "object":
            cls, _ = r.choice(self.classes)
            name = self.fresh("o")
            self.emit(indent, f"let {name} = {cls}({self.int_expr(scope)}, String(\"{name}\"))")
            scope.objects.append(name)
        elif kind == "object_use" and scope.objects:
            obj = r.choice(scope.objects)
            self.emit(indent, f"{obj}.bump()")
            self.emit(indent, f"print({obj}.n, {obj}.label)")
        elif kind == "list":
            self.imports.add("list")
            name = self.fresh("xs")
            self.emit(indent, f"let {name} = List[int]()")
            for _ in range(r.randint(1, 4)):
                self.emit(indent, f"{name}.push({self.int_expr(scope)})")
            scope.lists.append(name)
        elif kind == "list_use" and scope.lists:
            xs = r.choice(scope.lists)
            self.emit(indent, f"{xs}[0] = {self.int_expr(scope)}")
            self.emit(indent, f"{xs}[len({xs}) - 1] += 1")
            v = self.fresh("v")
            self.emit(indent, f"for {v} in {xs}:")
            self.emit(indent + 1, f"print(\"item\", {v})")
        elif kind == "string":
            self.imports.update(["string", "list"])
            s, t, bag = self.fresh("s"), self.fresh("t"), self.fresh("bag")
            self.emit(indent, f"let {s} = String(\"{s}\")")
            self.emit(indent, f"{s}.append(\"!\")")
            self.emit(indent, f"let {t} = {s}.copy()")
            self.emit(indent, f"let {bag} = List[String]()")
            self.emit(indent, f"{bag}.push({s})")            # s is moved here
            self.emit(indent, f"print({bag}[0], {t}, len({bag}))")
        elif kind == "generator":
            gen = r.choice(self.generators)
            v = self.fresh("y")
            self.emit(indent, f"for {v} in {gen}({r.randint(0, 6)}):")
            inner = scope.child(loop=True)
            inner.ints.append(v)
            self.emit(indent + 1, f"print(\"gen\", {v})")
            if r.random() < 0.5:
                self.emit(indent + 1, f"if {v} >= {r.randint(0, 6)}:")
                self.emit(indent + 2, "break")
        elif kind == "lambda":
            name = self.fresh("fn")
            captured = r.choice(scope.ints) if scope.ints else "1"
            if r.random() < 0.5:
                self.emit(indent, f"let {name} = lambda (p: int): (p + {captured}) % 1000")
                scope.lambdas.append((name, 1))
            else:
                self.emit(indent, f"let {name} = lambda p, q: (p * 2 + q + {captured}) % 1000")
                scope.lambdas.append((name, 2))
        elif kind == "enum":
            enum, cases = r.choice(self.enums)
            e = self.fresh("e")
            self.emit(indent, f"let {e} = {enum}.{r.choice(cases)}({self.int_expr(scope)})")
            self.emit(indent, f"switch {e}:")
            for case in cases:
                self.emit(indent + 1, f"case {case}(v):")
                self.emit(indent + 2, f"print(\"{case}\", v)")
        elif kind == "optional":
            o = self.fresh("opt")
            self.emit(indent, f"let {o}: ?int = when {self.cond(scope)} use {self.int_expr(scope)} otherwise none")
            self.emit(indent, f"print({o}.value_or(-1), {o} is none)")
        else:
            self.emit(indent, f"print({self.int_expr(scope)})")

    def build(self):
        r = self.rng
        for _ in range(r.randint(0, 2)):
            self.add_class()
        for _ in range(r.randint(1, 3)):
            self.add_function()
        for _ in range(r.randint(0, 2)):
            self.add_generator()
        for _ in range(r.randint(0, 1)):
            self.add_enum()

        self.lines.append("function main() -> int32:")
        scope = Scope()
        for _ in range(r.randint(1, 2)):
            name = self.fresh("m")
            self.emit(1, f"let {name} = {r.randint(0, 50)}")
            scope.ints.append(name)
            scope.mutable.append(name)
        self.block(scope, 1, 0, r.randint(4, 12))
        self.emit(1, "return 0")

        header = [f"import \"{name}\"" for name in sorted(self.imports)]
        return "\n".join(header + [""] + self.lines) + "\n"


class Scope:
    def __init__(self, ints=None, parent=None, loop=False):
        self.ints = list(ints or (parent.ints if parent else []))
        self.mutable = list(parent.mutable) if parent else []
        self.lists = list(parent.lists) if parent else []
        self.objects = list(parent.objects) if parent else []
        self.lambdas = list(parent.lambdas) if parent else []

    def child(self, loop=False):
        return Scope(parent=self, loop=loop)


def run(cmd, **kwargs):
    try:
        return subprocess.run(cmd, capture_output=True, text=True, timeout=60, stdin=subprocess.DEVNULL, **kwargs)
    except subprocess.TimeoutExpired:
        return None


def check(clearc, source_path, workdir):
    """Returns (problem kind, details) or (None, None)."""
    outputs = []
    for level in ("-O0", "-O3"):
        binary = os.path.join(workdir, "program" + level)
        compiled = run([clearc, "build", source_path, "-o", binary, level])

        if compiled is None:
            return "crash", f"compiler timed out at {level}"
        output = compiled.stdout + compiled.stderr
        if compiled.returncode < 0 or "internal compiler error" in output:
            return "crash", f"at {level}:\n{output}"
        if compiled.returncode != 0:
            return "rejected", f"at {level}:\n{output}"

        result = run([binary])
        if result is None:
            return "runtime", f"timed out at {level}"
        if result.returncode != 0:
            return "runtime", f"exit code {result.returncode} at {level}:\n{result.stdout}{result.stderr}"
        outputs.append(result.stdout)

    if outputs[0] != outputs[1]:
        return "mismatch", "--- -O0\n" + outputs[0] + "--- -O3\n" + outputs[1]
    return None, None


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("clearc")
    parser.add_argument("--count", type=int, default=100)
    parser.add_argument("--seed", type=int, default=None, help="first seed (each program uses the next one)")
    parser.add_argument("--out", default="fuzz-failures")
    parser.add_argument("--keep", action="store_true", help="also keep the programs that passed")
    args = parser.parse_args()

    clearc = os.path.abspath(args.clearc)
    first = args.seed if args.seed is not None else random.randrange(1 << 30)
    found = {}

    with tempfile.TemporaryDirectory(prefix="clear-fuzz-") as workdir:
        for seed in range(first, first + args.count):
            source = Program(random.Random(seed)).build()
            source_path = os.path.join(workdir, f"fuzz_{seed}.cl")
            with open(source_path, "w") as f:
                f.write(source)

            kind, details = check(clearc, source_path, workdir)

            if kind or args.keep:
                os.makedirs(args.out, exist_ok=True)
                shutil.copy(source_path, args.out)
            if kind:
                found.setdefault(kind, []).append(seed)
                with open(os.path.join(args.out, f"fuzz_{seed}.txt"), "w") as f:
                    f.write(f"{kind}\n{details}")
                print(f"seed {seed}: {kind}")

    total = sum(len(seeds) for seeds in found.values())
    print(f"\n{args.count} programs from seed {first}: {total} problem(s)")
    for kind, seeds in sorted(found.items()):
        print(f"  {kind}: {len(seeds)} (seeds {', '.join(map(str, seeds[:10]))}{' ...' if len(seeds) > 10 else ''})")
    if total:
        print(f"programs and details are in {args.out}/")
    return 1 if total else 0


if __name__ == "__main__":
    sys.exit(main())
