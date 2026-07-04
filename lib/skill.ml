let skill_text = {|---
name: calculator
description: Evaluate math expressions using the industrial calculator CLI
---

# Calculator

Pipe an expression to the `calculator` binary to evaluate it:

    echo "1 + 2 * 3" | calculator

Supported syntax:
- operators: `+` `-` `*` `/` and `^` (exponent, right-associative)
- parentheses for grouping
- functions: `sin`, `cos`, `sqrt`

Running `calculator` with no input starts an interactive REPL;
type `/help` there to list its commands.
|}
