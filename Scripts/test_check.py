#!/usr/bin/env python3
"""Tests for `clearc check`, the command editors run on save.

    python3 Scripts/test_check.py build/clearc

check must accept every example, report errors with a file:line:column location,
and never write an executable.
"""

import os
import re
import subprocess
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent


def check(clearc, target, cwd):
    return subprocess.run([clearc, "check", str(target)], cwd=cwd, capture_output=True, text=True, timeout=120)


def main():
    if len(sys.argv) < 2:
        print("usage: test_check.py <clearc>")
        return 2

    clearc = str(Path(sys.argv[1]).resolve())
    failures = 0

    def expect(condition, name, detail=""):
        nonlocal failures
        print(("PASS " if condition else "FAIL ") + name)
        if not condition:
            failures += 1
            if detail:
                print(detail)

    with tempfile.TemporaryDirectory() as temp:
        temp = Path(temp)

        for example in sorted((ROOT / "examples").glob("*.cl")):
            result = check(clearc, example, temp)
            expect(result.returncode == 0, f"check {example.name}", result.stdout + result.stderr)

        expect(not any(temp.iterdir()), "check writes nothing", "\n".join(str(p) for p in temp.iterdir()))

        bad = temp / "bad.cl"
        bad.write_text("function main() -> int32:\n    let x: int = \"hi\"\n    print(missing)\n    return 0\n")
        result = check(clearc, bad, temp)
        output = result.stdout + result.stderr
        locations = re.findall(r"--> (.*):(\d+):(\d+)", output)

        expect(result.returncode == 1, "check fails on errors", output)
        expect([(int(l), int(c)) for _, l, c in locations] == [(2, 18), (3, 11)], "check reports every error with its location", output)
        expect(not (temp / "bad").exists(), "check does not build a failing file")

        # a file inside a project sees the project's dependencies
        subprocess.run([clearc, "new", "app"], cwd=temp, capture_output=True, check=True)
        library = temp / "shapes"
        library.mkdir()
        (library / "clear.toml").write_text('[package]\nname = "shapes"\nversion = "0.1.0"\nmain = "shapes.cl"\n')
        (library / "shapes.cl").write_text("function area(w: int, h: int) -> int:\n    return w * h\n")
        subprocess.run([clearc, "add", "shapes", "--path", str(library)], cwd=temp / "app", capture_output=True, check=True)

        nested = temp / "app" / "src"
        nested.mkdir()
        (nested / "use.cl").write_text('import "shapes"\n\nfunction main() -> int32:\n    print(area(2, 3))\n    return 0\n')

        result = check(clearc, nested / "use.cl", temp)
        expect(result.returncode == 0, "check a file inside a project", result.stdout + result.stderr)

        result = check(clearc, temp / "app", temp)
        expect(result.returncode == 0, "check a project directory", result.stdout + result.stderr)

    print(f"\n{failures} failure(s)" if failures else "\nall check tests passed")
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
