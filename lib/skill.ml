let skill_text = {|---
name: calculator
description: Evaluate math expressions using the industrial calculator CLI
---

# Calculator

Evaluate an expression in one shot with the `calculate` subcommand:

    calculator calculate "1 + 2 * 3"

Prints the bare result on stdout (exit 0), or an error on stderr (exit 1).

Supported syntax:
- operators: `+` `-` `*` `/` and `^` (exponent, right-associative)
- parentheses for grouping
- functions: `sin`, `cos`, `sqrt`

Running `calculator` with no input starts an interactive REPL;
type `/help` there to list its commands.
|}
