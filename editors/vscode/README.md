# Clear for Visual Studio Code

Language support for [Clear](https://github.com/clearlanguage/clear), the language that reads like Python and runs like C.

## Features

- **Syntax highlighting** for every construct: declarations, generics, operators, `when … use … otherwise`, macros (`name!(...)`), optionals (`?.`, `??`, `:=`), number literals, escapes. Also in ```` ```clear ```` blocks in Markdown.
- **Errors as you save**: the extension runs `clearc check` (type checking only, nothing is built) and shows every error and warning in the editor and the Problems panel, including errors in imported files. Set `clear.diagnostics.trigger` to `onType` to check while typing.
- **Completion**
  - variables, parameters and everything the file declares or imports, with signatures and doc comments
  - members after a dot, using the types the code implies: `points[0].`, `for p in points: p.`, `list.last().`, `self.`, `super.`, `Color.`, `geo.` (an `import ... as geo`)
  - standard library functions you have not imported yet: picking `sqrt` adds `import "math"`
  - module names and files inside `import "..."`
- **Hover**: signatures, doc comments (the `//` lines above a declaration), the inferred type of a `let`, and help for keywords and built-ins.
- **Go to definition** (also into the standard library and packages), **find references**, **highlight**, and **rename** (locals, parameters and your own declarations).
- **Signature help** for functions, methods, constructors (`Point(`), enum cases, macros and built-ins.
- **Inlay hints** with the type of `let` variables that don't write one (`clear.inlayHints.variableTypes`).
- **Quick fix** for an undefined name that a standard module provides: *Add import "math"*.
- **Outline**, breadcrumbs, **Go to Symbol in Workspace**, folding by indentation, and import paths as links.
- **Snippets** for functions, classes, enums, `for`, `switch`, `operator`s and more.
- **Commands**: *Clear: Run File* (`Ctrl+F5`, also the ▶ button in the editor title), *Build File*, *Run Project*, *New Project...*, *Show LLVM IR*, *Check File for Errors*.
- **Tasks** (`clear` type) for `run`, `build` and `check`, with a `$clearc` problem matcher.

## Requirements

`clearc` must be built (see the [Clear README](https://github.com/clearlanguage/clear#building)). The extension looks for it in this order:

1. the `clear.compilerPath` setting
2. `build/clearc` in the open folder (when you work on the compiler itself)
3. `clearc` on the `PATH`

Everything except errors and the run/build commands works without the compiler.

## Settings

| setting | default | |
| --- | --- | --- |
| `clear.compilerPath` | `""` | path to `clearc` |
| `clear.standardLibraryPath` | `""` | the `Standard/` folder used for completion; empty means `$CLEAR_STANDARD_DIR`, then `Standard/` in the workspace, then the copy bundled with the extension |
| `clear.diagnostics.enable` | `true` | show errors from `clearc check` |
| `clear.diagnostics.trigger` | `"onSave"` | `onSave`, or `onType` to also check unsaved text |
| `clear.inlayHints.variableTypes` | `true` | show inferred `let` types |
| `clear.run.arguments` | `[]` | extra options for run/build, e.g. `["-O3"]` |

## Building the extension

```
cd editors/vscode
npm install
npm test                # unit tests: grammar, completion, hover, references...
npm run package         # writes clear-language-<version>.vsix
code --install-extension clear-language-0.1.0.vsix
```

To try changes without installing, open `editors/vscode` in VS Code and press `F5` (*Run Extension*).

## How it works

Completion, hover and navigation come from a small reader of Clear source written in TypeScript (`src/analyzer.ts`, `src/project.ts`), which finds declarations and works out types well enough to suggest members. It follows imports the way `clearc` does: next to the file, then the project's packages (`clear.toml`), then the standard library. Errors come from the real compiler, so they are always exactly what `clearc build` would say.
