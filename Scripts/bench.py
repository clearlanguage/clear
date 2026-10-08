#!/usr/bin/env python3
"""Compare Clear against equivalent C programs.

For every Benchmarks/<name>.cl with a matching <name>.c, both are built with
full optimization and run a few times; the best wall time is reported.

usage: bench.py <path to clearc> [benchmarks dir] [runs]
"""

import os
import subprocess
import sys
import tempfile
import time


def best_time(executable, runs):
    best, output = None, ""
    for _ in range(runs):
        start = time.perf_counter()
        result = subprocess.run([executable], capture_output=True, text=True, timeout=300)
        elapsed = time.perf_counter() - start
        output = result.stdout.strip()
        best = elapsed if best is None else min(best, elapsed)
    return best, output


def main():
    if len(sys.argv) < 2:
        print(__doc__)
        return 2

    clearc = os.path.abspath(sys.argv[1])
    bench_dir = sys.argv[2] if len(sys.argv) > 2 else os.path.join(os.path.dirname(__file__), "..", "Benchmarks")
    runs = int(sys.argv[3]) if len(sys.argv) > 3 else 3
    cc = os.environ.get("CC", "clang")

    names = sorted(f[:-3] for f in os.listdir(bench_dir) if f.endswith(".cl"))
    print(f"{'benchmark':<12}{'clear (s)':>12}{'c (s)':>10}{'ratio':>9}  output")

    with tempfile.TemporaryDirectory(prefix="clear-bench-") as work:
        for name in names:
            clear_bin = os.path.join(work, name + "_clear")
            c_bin = os.path.join(work, name + "_c")

            build = subprocess.run([clearc, "build", os.path.join(bench_dir, name + ".cl"), "-O3", "-o", clear_bin],
                                   capture_output=True, text=True)
            if build.returncode != 0:
                print(f"{name:<12} failed to compile:\n{build.stdout}{build.stderr}")
                continue

            clear_time, clear_out = best_time(clear_bin, runs)

            c_source = os.path.join(bench_dir, name + ".c")
            if os.path.exists(c_source):
                subprocess.run([cc, "-O3", c_source, "-o", c_bin, "-lm"], check=True)
                c_time, c_out = best_time(c_bin, runs)
                same = "" if c_out == clear_out else f"  (C printed {c_out})"
                print(f"{name:<12}{clear_time:>12.3f}{c_time:>10.3f}{clear_time / c_time:>8.2f}x  {clear_out}{same}")
            else:
                print(f"{name:<12}{clear_time:>12.3f}{'-':>10}{'-':>9}  {clear_out}")

    return 0


if __name__ == "__main__":
    sys.exit(main())
