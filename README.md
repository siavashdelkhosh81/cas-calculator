<div align="center">

# ⊞ cas-calculator

**A fast, open-source computer algebra system — SymPy's power, native-compiled speed.**

*Lex → parse → an algebra engine, written in OCaml.*

[![OCaml](https://img.shields.io/badge/OCaml-5.x-EC6813?logo=ocaml&logoColor=white)](https://ocaml.org)
[![Build](https://img.shields.io/badge/build-dune-blueviolet)](https://dune.build)
[![Status](https://img.shields.io/badge/status-early%20development-orange)]()
[![License](https://img.shields.io/badge/license-TBD-lightgrey)]()

</div>

---

## Why

[SymPy](https://www.sympy.org) is the reference open-source CAS — but it runs on Python, and symbolic work is exactly where an interpreter hurts most. The goal of this project is a CAS that feels like SymPy to use but is **compiled to native code**, so simplification, differentiation, and evaluation are fast by default.

OCaml is the right tool: algebraic data types model an expression tree perfectly, pattern matching makes rewrite rules concise, and the native compiler gives C-class speed with memory safety.

> **Status — early development.** Today this is a working numeric expression engine (the lexer → parser → AST → evaluator pipeline). The *symbolic* layer that makes it a true CAS is on the [roadmap](#roadmap) below. This README describes both what exists now and where it's going; the [feature table](#features) marks each clearly.

---

## Quick start

Requires [OCaml](https://ocaml.org/install) (5.x) and [dune](https://dune.build).

```bash
# clone
git clone https://github.com/youruser/cas-calculator.git
cd cas-calculator

# build
dune build

# launch the REPL
dune exec calculator
```

You'll get an interactive prompt:

```
╔══════════════════════════════════════════════════════════════════════════════════╗
║   ██████╗ █████╗ ██╗      ██████╗██╗   ██╗██╗      █████╗ ████████╗ ██████╗ ██████╗ ║
║  ██╔════╝██╔══██╗██║     ██╔════╝██║   ██║██║     ██╔══██╗╚══██╔══╝██╔═══██╗██╔══██╗║
║  ...                                                                               ║
╠══════════════════════════════════════════════════════════════════════════════════╣
║  INDUSTRIAL CALCULATOR · v1.0.0 · arbitrary precision core                         ║
║  type an expression — /q to quit, /help for help                                   ║
╚══════════════════════════════════════════════════════════════════════════════════╝

▸ 2 + 3 * 4
= 14
▸ (2 + 3) * 4
= 20
▸ /q
bye
```

### REPL commands

| Command  | Action                          |
|----------|---------------------------------|
| `/help`  | list available commands         |
| `/clear` | clear the screen                |
| `/q`     | quit                            |

Anything else is parsed and evaluated as an expression.

---

## Features

| Capability                                   | Status        |
|----------------------------------------------|---------------|
| Tokenizer (numbers, identifiers, operators)  | ✅ done        |
| Recursive-descent parser with precedence     | ✅ done        |
| `+`, `*`, parentheses                         | ✅ done        |
| Numeric evaluation                           | ✅ done        |
| Typed, recoverable errors (`result`)         | ✅ done        |
| `-`, `/`, `^` (power), unary minus           | 🔜 next        |
| Functions (`sin`, `cos`, `sqrt`, `log`, …)   | 🔜 next        |
| Variable bindings (`let x = 3`)              | 🔜 next        |
| Exact rationals / bignums (no float error)   | 🗺️ planned     |
| **Symbolic simplification** (`x + x → 2x`)   | 🗺️ planned     |
| **Differentiation** (`d/dx`)                 | 🗺️ planned     |
| Expansion / factoring                        | 🗺️ planned     |
| Equation solving                             | 🗺️ planned     |

Errors never crash the REPL — every failure is a typed code (`Invalid_char`, `Unexpected_token`, `Unbound_variable`, …) surfaced as a clean message:

```
▸ 1.2.3
error: invalid number: 1.2.3
▸ (1 + 2
error: missing closing ')'
▸ @
error: invalid character: '@'
```

---

## How it works

The classic compiler front-end pipeline:

```
input string  →  lexer  →  tokens  →  parser  →  tree (AST)  →  eval  →  result
```

| Stage      | File              | Job                                                        |
|------------|-------------------|------------------------------------------------------------|
| **Lexer**  | `lib/lexer.ml`    | turn the raw string into a flat list of tokens             |
| **Parser** | `lib/parser.ml`   | recursive descent: tokens → an expression tree, honoring precedence and grouping |
| **AST**    | `lib/ast.ml`      | the `expr` tree type (`Num`, `Var`, `Add`, `Mul`, …)        |
| **Eval**   | `lib/eval.ml`     | walk the tree to a value; the future home of the CAS engine |
| **Errors** | `lib/error.ml`    | shared error codes, raised internally, returned as `result`|

The parser is the heart of the front end. **An interactive, step-by-step visualization of how it builds the tree lives in [`docs/parser-explained.html`](docs/parser-explained.html)** — open it in a browser to watch the call stack and AST grow token by token.

---

## Roadmap

The path from "calculator" to "CAS", in order:

1. **Complete the numeric calculator** — `-`, `/`, `^`, unary minus, built-in functions, variable bindings.
2. **Exact arithmetic** — replace `float` with rationals/bignums ([`zarith`](https://github.com/ocaml/Zarith)), so `1/3` stays `1/3`. A real CAS must be exact.
3. **Simplification engine** — canonical forms, constant folding, identities (`x*1 → x`, `x + x → 2x`). This is the core of a CAS.
4. **Differentiation** — symbolic `d/dx`, piped through the simplifier.
5. **Expansion & factoring** — polynomial arithmetic, `(x+1)² ↔ x² + 2x + 1`.
6. **Solving** — linear, then polynomial equations.

Contributions toward any of these are very welcome — see below.

---

## Project structure

```
.
├── bin/
│   └── main.ml          # REPL entry point (banner, prompt, command dispatch)
├── lib/                 # the calculator library
│   ├── lexer.ml         # string  → tokens
│   ├── parser.ml        # tokens  → AST
│   ├── ast.ml           # the expression tree type
│   ├── eval.ml          # AST     → result   (future CAS engine)
│   ├── error.ml         # shared error codes
│   ├── banner.ml        # REPL banner / prompt
│   └── commands.ml      # /help, /clear, …
├── docs/
│   └── parser-explained.html   # interactive parser visualizer
├── test/
└── dune-project
```

---

## Development

```bash
dune build            # compile
dune exec calculator  # run the REPL
dune test             # run tests
dune fmt              # format (requires ocamlformat)
```

The architecture is deliberately layered: each stage has a small `.mli` interface, so you can work on the parser without touching the lexer, or swap the evaluator without disturbing parsing.

---

## Contributing

This is an early, open project — the best time to shape it. Good first contributions:

- Add a missing operator (`-`, `/`, `^`) end-to-end: AST node → lexer → parser → eval → error code.
- Add a built-in function.
- Expand the test suite in `test/`.
- Take on a roadmap item (open an issue first to coordinate).

Adding an operator touches all four stages but each change is small and local — a good way to learn the codebase.

---

## License

To be decided. (Open source — a permissive license such as MIT is the likely choice.)
