#!/usr/bin/env python3
"""End-to-end test of clearc's package manager, using local git repositories.

usage: test_packages.py <path to clearc>
"""

import os
import subprocess
import sys
import tempfile


def run(*args, cwd=None, check=True):
    result = subprocess.run(list(args), cwd=cwd, capture_output=True, text=True, timeout=120)
    if check and result.returncode != 0:
        raise AssertionError(f"{' '.join(args)} failed ({result.returncode}):\n{result.stdout}{result.stderr}")
    return result


def git(*args, cwd):
    return run("git", "-c", "user.name=test", "-c", "user.email=test@example.com", "-c", "init.defaultBranch=main", *args, cwd=cwd)


def write(path, text):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w") as f:
        f.write(text)


def main():
    clearc = os.path.abspath(sys.argv[1])

    with tempfile.TemporaryDirectory(prefix="clear-packages-") as root:
        # a git package with two tagged versions, a second file, and a path package inside its repository
        colors = f"{root}/colors"
        write(f"{colors}/palette/clear.toml", '[package]\nname = "palette"\nlib = "palette.cl"\n')
        write(f"{colors}/palette/palette.cl", 'function base() -> int:\n    return 100\n')
        write(f"{colors}/clear.toml", '[package]\nname = "colors"\n\n[dependencies]\npalette = { path = "palette" }\n')
        write(f"{colors}/colors.cl", 'import "palette"\n\nfunction red() -> int:\n    return base() + 1\n')
        write(f"{colors}/extra.cl", 'function blue() -> int:\n    return 3\n')
        git("init", "--quiet", cwd=colors)
        git("add", ".", cwd=colors)
        git("commit", "--quiet", "-m", "v1", cwd=colors)
        git("tag", "v1.0", cwd=colors)

        write(f"{colors}/colors.cl", 'import "palette"\n\nfunction red() -> int:\n    return base() + 2\n')
        git("commit", "--quiet", "-am", "v2", cwd=colors)
        git("tag", "v2.0", cwd=colors)

        # the application
        app = f"{root}/app"
        run(clearc, "new", app)
        assert os.path.exists(f"{app}/clear.toml") and os.path.exists(f"{app}/main.cl")
        assert "hello from app" in run(clearc, "run", app).stdout

        run(clearc, "add", "colors", "--git", f"file://{colors}", "--tag", "v1.0", cwd=app)
        write(f"{app}/main.cl", 'import "colors"\nimport "colors/extra"\n\nfunction main() -> int32:\n    print(red(), blue())\n    return 0\n')

        output = run(clearc, "run", app).stdout.strip()
        assert output == "101 3", output

        lock = open(f"{app}/clear.lock").read()
        assert 'name = "colors"' in lock and "commit = " in lock, lock

        # the lock keeps v1.0 even after the manifest asks for v2.0, until `update`
        manifest = open(f"{app}/clear.toml").read()
        assert manifest.startswith("[package]") and "# shapes" in manifest, manifest   # the user's layout is kept
        manifest = manifest.replace('"v1.0"', '"v2.0"')
        write(f"{app}/clear.toml", manifest)
        assert run(clearc, "run", app).stdout.strip() == "101 3"

        run(clearc, "update", app)
        output = run(clearc, "run", app)
        assert output.stdout.strip() == "102 3", output.stdout + output.stderr

        # a fresh checkout (no .clear/) gets exactly the locked version back
        run("rm", "-rf", f"{app}/.clear")
        run(clearc, "fetch", app)
        assert run(clearc, "run", app).stdout.strip() == "102 3"

        # build puts the program in build/
        run(clearc, "build", app)
        assert run(f"{app}/build/app").stdout.strip() == "102 3"

        # -o keeps the name exactly as given, dots included, and leaves no object file behind
        write(f"{root}/single/hello.cl", 'function main() -> int32:\n    print("hi")\n    return 0\n')
        for name in ("app.v2", "out.exe", "bin/r1.O0"):
            target = f"{root}/single/{name}"
            os.makedirs(os.path.dirname(target), exist_ok=True)
            run(clearc, "build", f"{root}/single/hello.cl", "-o", target)
            assert run(target).stdout.strip() == "hi", name
        left = sorted(f for _, _, files in os.walk(f"{root}/single") for f in files if f.endswith(".o"))
        assert not left, left

        # adding the same name twice is refused
        again = run(clearc, "add", "colors", "--path", "../x", cwd=app, check=False)
        assert again.returncode != 0 and "already a dependency" in again.stderr, again.stderr

        # errors are reported, not crashes
        bad = run(clearc, "add", "nothing", "--git", f"file://{root}/missing", cwd=app, check=False)
        assert bad.returncode != 0 and "could not clone" in bad.stderr, bad.stderr

        # ...with git's own reason, not just its last line
        assert "does not appear to be a git repository" in bad.stderr, bad.stderr
        assert "Please make sure you have the correct access rights and the repository exists" in bad.stderr, bad.stderr

        # fetch reports every broken dependency, not only the first
        broken = f"{root}/broken"
        write(f"{broken}/clear.toml", '[package]\nname = "broken"\n\n[dependencies]\n'
              f'one = {{ git = "file://{root}/missing1" }}\n'
              f'two = {{ git = "file://{root}/missing2" }}\n'
              'three = { path = "../nowhere" }\n')
        write(f"{broken}/main.cl", 'function main() -> int32:\n    return 0\n')
        fetched = run(clearc, "fetch", broken, check=False)
        assert fetched.returncode != 0, fetched.stdout + fetched.stderr
        for name in ("'one'", "'two'", "'three'"):
            assert name in fetched.stderr, fetched.stderr

        # add refuses conflicting or unknown options and bad names, and leaves clear.toml alone
        before = open(f"{app}/clear.toml").read()
        for args, message in (
            (("x", "--git", f"file://{colors}", "--tag", "v1.0", "--rev", "HEAD~1"), "at most one of --tag, --branch and --rev"),
            (("x", "--git", f"file://{colors}", "--frob"), "unknown option '--frob'"),
            (("bad name!", "--git", f"file://{colors}"), "not a valid package name"),
            (("x", "--git", f"file://{colors}", "--path", "../x"), "not both"),
            (("x", "--path", "../x", "--tag", "v1.0"), "only apply to a --git dependency"),
        ):
            refused = run(clearc, "add", *args, cwd=app, check=False)
            assert refused.returncode != 0 and message in refused.stderr, refused.stderr
        assert open(f"{app}/clear.toml").read() == before

        # a hand-written manifest with two selectors is refused too
        two = f"{root}/two_selectors"
        write(f"{two}/clear.toml", f'[package]\nname = "two"\n\n[dependencies]\ncolors = {{ git = "file://{colors}", tag = "v1.0", branch = "main" }}\n')
        write(f"{two}/main.cl", 'function main() -> int32:\n    return 0\n')
        refused = run(clearc, "fetch", two, check=False)
        assert refused.returncode != 0 and "more than one of tag, branch and rev" in refused.stderr, refused.stderr

        # an update that fails leaves the checkout at the locked commit
        pinned = f"{root}/pinned"
        write(f"{pinned}/clear.toml", f'[package]\nname = "pinned"\n\n[dependencies]\ncolors = {{ git = "file://{colors}", tag = "v1.0" }}\n')
        write(f"{pinned}/main.cl", 'function main() -> int32:\n    return 0\n')
        run(clearc, "fetch", pinned)
        checkout = f"{pinned}/.clear/packages/colors"
        locked = git("rev-parse", "HEAD", cwd=checkout).stdout.strip()
        write(f"{pinned}/clear.toml", f'[package]\nname = "pinned"\n\n[dependencies]\ncolors = {{ git = "file://{colors}" }}\nmissing = {{ git = "file://{root}/missing" }}\n')
        failed = run(clearc, "update", pinned, check=False)
        assert failed.returncode != 0, failed.stdout + failed.stderr
        assert git("rev-parse", "HEAD", cwd=checkout).stdout.strip() == locked

    print("package manager: all checks passed")
    return 0


if __name__ == "__main__":
    sys.exit(main())
