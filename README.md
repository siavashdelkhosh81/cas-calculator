<div align="center">

# ⊞ cas-calculator

**A fast, open-source computer algebra system — native-compiled speed, built in OCaml.**

*Lex → parse → an algebra engine, written in OCaml.*

[![OCaml](https://img.shields.io/badge/OCaml-5.x-EC6813?logo=ocaml&logoColor=white)](https://ocaml.org)
[![Build](https://img.shields.io/badge/build-dune-blueviolet)](https://dune.build)
[![License](https://img.shields.io/badge/license-Apache_2.0-lightgrey)]()

</div>

<img width="934" height="478" alt="image" src="https://github.com/user-attachments/assets/7ae7edda-cd26-4616-820d-0114d87f9309" />

---

## Why

I wanted to learn OCaml, so I started with the basics and built myself a full Computer Algebra System (CAS). OCaml proved to be the perfect tool for the job: algebraic data types naturally model expression trees, pattern matching makes mathematical rewrite rules concise, and the native compiler provides C-class speed alongside memory safety.

---

## Quick Start

Requires [OCaml](https://ocaml.org/install) (5.x) and [dune](https://dune.build).

```bash
# Clone the repository
git clone https://github.com/siavashdelkhosh81/cas-calculator.git
cd cas-calculator

# Build the project
dune build

# Launch the REPL
dune exec calculator

```

You will be greeted with an interactive prompt:

You can also evaluate a single expression directly from your terminal without entering the REPL:

```bash
dune exec -- calculator calculate "-1 + 2"
# 1

```

### REPL Commands

| Command | Action |
| --- | --- |
| `/help` | List available commands |
| `/clear` | Clear the screen |
| `/install_skill` | Install the calculator skill into AI tools (`~/.claude`) |
| `/q` | Quit the REPL |

Any other input is parsed and evaluated as a mathematical expression.

---

## Features

The calculator features a robust evaluation and symbolic pipeline, built to safely handle complex expressions without crashing. Errors are typed and surfaced clearly (e.g., `Invalid_char`, `Unexpected_token`, `Unbound_variable`).

* **Core Parsing:** Tokenizer and recursive-descent parser with proper precedence mapping.
* **Arithmetic:** `+`, `-`, `*`, `/`, `^` (power), and parentheses.
* **Exact Arithmetic:** Built on exact rationals and bignums (no floating-point rounding errors).
* **Functions:** Native support for `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `sinh`, `cosh`, `tanh`, `sqrt`, `exp`, `ln`, `log`/`log10`, `log2`, `abs`, `floor`, `ceil`, `round`.
* **Variable Bindings:** Supports local environment assignments (e.g., `let x = 3`).
* **Symbolic Mathematics:**
* **Simplification:** Canonical forms and constant folding (e.g., `x + x → 2x`).
* **Differentiation:** Symbolic derivatives via `d/dx`.
* **Expansion & Factoring:** Polynomial arithmetic and structural expansion.



---

## Architecture

The system follows a classic compiler front-end pipeline:

`input string` → `lexer` → `tokens` → `parser` → `AST` → `evaluator` → `result`

| Stage | File | Responsibility |
| --- | --- | --- |
| **Lexer** | `lib/lexer.ml` | Transforms raw input strings into a flat stream of tokens. |
| **Parser** | `lib/parser.ml` | Recursive descent parsing; structures tokens into an AST based on precedence. |
| **AST** | `lib/ast.ml` | Defines the `expr` tree (`Num`, `Var`, `Add`, `Sub`, `Mul`, `Div`, `Func`, etc). |
| **Eval** | `lib/eval.ml` | Traverses the AST to simplify, differentiate, or evaluate the mathematical result. |
| **Errors** | `lib/calc_error.ml` | Typed error management; prevents runtime crashes and ensures safe failure. |

> **Note:** An interactive, step-by-step visualization of how the parser builds the AST is available in [`docs/parser-explained.html`](https://www.google.com/search?q=docs/parser-explained.html). Open it in your browser to observe the call stack in action.

---

## Project Structure

```text
.
├── bin/
│   └── main.ml          # REPL entry point (banner, prompt, command dispatch)
├── lib/                 # Core calculator library
│   ├── lexer.ml         # String → Tokens
│   ├── parser.ml        # Tokens → AST
│   ├── ast.ml           # Expression tree types
│   ├── eval.ml          # AST → Result (CAS engine)
│   ├── calc_error.ml    # Shared error codes and handling
│   ├── banner.ml        # REPL UI/UX components
│   ├── commands.ml      # REPL command routing
│   ├── skill.ml         # Skill documentation generation
│   └── fs.ml            # Filesystem utilities
├── docs/
│   └── parser-explained.html 
├── test/                # Unit and integration tests
└── dune-project

```

---

## Development

The architecture is deliberately modular. Each stage exposes a strict `.mli` interface, meaning you can iterate on the parser without affecting the lexer, or enhance the evaluation engine without disturbing the AST structure.

```bash
dune build            # Compile the project
dune exec calculator  # Run the interactive REPL
dune test             # Execute the test suite
dune fmt              # Format code (requires ocamlformat)

```

---

## Contributing

Contributions are highly encouraged! Whether you are fixing a bug, expanding the documentation, or adding a new mathematical capability, your help is welcome.

**To contribute:**

1. Fork the repository.
2. Create a new branch for your feature (`git checkout -b feature/amazing-feature`).
3. Commit your changes (`git commit -m 'Add amazing feature'`).
4. Ensure all tests pass (`dune test`) and code is formatted (`dune fmt`).
5. Push to your branch (`git push origin feature/amazing-feature`).
6. Open a Pull Request.

For significant architectural changes or major new algebraic capabilities, please open an Issue first to discuss your proposed approach.

---

## License

Distributed under the APACHE License. See `LICENSE` for more information.

```
