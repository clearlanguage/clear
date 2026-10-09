#!/usr/bin/env python3
"""Clear test runner.

Every ``*.cl`` file under the tests directory is one test. The expected result
is written in comments inside the test itself:

    // expect:
    // first line of stdout
    // second line of stdout

    // expect-exit: 3        (optional, default 0)
    // expect-error          (compilation must fail)
    // expect-error: E094    (... with this text in the compiler's output, e.g. an error code)
    // expect-warning: E099  (compilation must succeed and print this text)
    // flags: --checks       (extra compiler flags for this test, after CLEAR_TEST_FLAGS)

Files inside a folder named `lib` are helpers that tests import, not tests.

usage: run_tests.py <path to clearc> <tests directory> [name filter]

Set CLEAR_TEST_FLAGS to pass extra compiler flags, e.g. CLEAR_TEST_FLAGS=-O3
"""

import os
import subprocess
import sys
import tempfile


def parse_expectations(path):
    expected_lines = None
    expected_exit = 0
    expect_error = False
    error_text = None
    warning_text = None
    flags = []

    with open(path, encoding="utf-8") as f:
        lines = f.read().splitlines()

    in_block = False
    for line in lines:
        stripped = line.strip()

        if stripped == "// expect:":
            expected_lines = []
            in_block = True
            continue

        if stripped.startswith("// expect-exit:"):
            expected_exit = int(stripped.split(":", 1)[1])
            in_block = False
            continue

        if stripped.startswith("// flags:"):
            flags = stripped.split(":", 1)[1].split()
            in_block = False
            continue

        if stripped == "// expect-error":
            expect_error = True
            in_block = False
            continue

        if stripped.startswith("// expect-error:"):
            expect_error = True
            error_text = stripped.split(":", 1)[1].strip()
            in_block = False
            continue

        if stripped.startswith("// expect-warning:"):
            warning_text = stripped.split(":", 1)[1].strip()
            in_block = False
            continue

        if in_block:
            if stripped.startswith("//"):
                # keep everything after "// " exactly, so leading spaces in output are testable
                content = stripped[2:]
                expected_lines.append(content[1:] if content.startswith(" ") else content)
            else:
                in_block = False

    return expected_lines, expected_exit, expect_error, error_text, warning_text, flags


def run_test(clearc, path, workdir):
    expected_lines, expected_exit, expect_error, error_text, warning_text, test_flags = parse_expectations(path)
    output = os.path.join(workdir, os.path.basename(path).removesuffix(".cl"))

    extra_flags = os.environ.get("CLEAR_TEST_FLAGS", "").split()

    compile_result = subprocess.run(
        [clearc, "build", path, "-o", output, *extra_flags, *test_flags],
        capture_output=True, text=True, timeout=30,
    )

    if compile_result.returncode < 0:
        return False, f"compiler crashed (signal {-compile_result.returncode}):\n" + compile_result.stdout + compile_result.stderr

    compiler_output = compile_result.stdout + compile_result.stderr

    if expect_error:
        if compile_result.returncode == 0:
            return False, "expected a compile error but compilation succeeded"
        if error_text and error_text not in compiler_output:
            return False, f"expected an error mentioning '{error_text}', got:\n" + compiler_output
        return True, ""

    if compile_result.returncode != 0:
        return False, "compilation failed:\n" + compiler_output

    if warning_text and warning_text not in compiler_output:
        return False, f"expected a warning mentioning '{warning_text}', got:\n" + compiler_output

    run_result = subprocess.run([output], capture_output=True, text=True, timeout=60, stdin=subprocess.DEVNULL)

    problems = []
    if run_result.returncode != expected_exit:
        problems.append(f"exit code {run_result.returncode}, expected {expected_exit}")

    if expected_lines is not None:
        actual_lines = run_result.stdout.splitlines()
        if actual_lines != expected_lines:
            problems.append(
                "output mismatch\n--- expected\n" + "\n".join(expected_lines)
                + "\n--- actual\n" + "\n".join(actual_lines)
            )

    return not problems, "\n".join(problems)


def main():
    if len(sys.argv) < 3:
        print(__doc__)
        return 2

    clearc = os.path.abspath(sys.argv[1])
    tests_dir = sys.argv[2]
    name_filter = sys.argv[3] if len(sys.argv) > 3 else ""

    tests = []
    for root, dirs, files in os.walk(tests_dir):
        # files under a `lib` folder are imported by tests, they are not tests themselves
        dirs[:] = [d for d in dirs if d != "lib"]

        for name in files:
            if name.endswith(".cl"):
                path = os.path.join(root, name)
                if name_filter in path:
                    tests.append(path)
    tests.sort()

    failures = []
    with tempfile.TemporaryDirectory(prefix="clear-tests-") as workdir:
        for path in tests:
            rel = os.path.relpath(path, tests_dir)
            try:
                ok, message = run_test(clearc, path, workdir)
            except subprocess.TimeoutExpired:
                ok, message = False, "timed out"

            print(("PASS " if ok else "FAIL ") + rel)
            if not ok:
                failures.append((rel, message))

    for rel, message in failures:
        print(f"\n=== {rel}\n{message}")

    print(f"\n{len(tests) - len(failures)}/{len(tests)} tests passed")
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
