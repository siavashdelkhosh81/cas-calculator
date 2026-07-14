let skill_text = {|---
name: calculator
description: Evaluate, simplify, differentiate, expand, factor, and solve math expressions using the industrial calculator CLI — a native computer algebra system (CAS)
---

# Calculator

A fast computer algebra system (CAS): it does exact arithmetic and real
symbolic math, not just floating-point evaluation.

Evaluate an expression in one shot with the `calculate` subcommand:

    calculator calculate "1 + 2 * 3"

Prints the bare result on stdout (exit 0), or an error on stderr (exit 1).

## Syntax

- operators: `+` `-` `*` `/` and `^` (exponent, right-associative)
- parentheses for grouping
- functions: `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `sinh`, `cosh`,
  `tanh`, `sqrt`, `exp`, `ln`, `log` (base 10, also `log10`), `log2`,
  `abs`, `floor`, `ceil`, `round`

## Exact arithmetic

Numbers are exact rationals and arbitrary-size integers — no floating-point
rounding. Results stay exact and are shown as fractions or big integers:

    calculator calculate "1/3 + 1/6"      # 1/2
    calculator calculate "2^100"          # 1267650600228229401496703205376
    calculator calculate "0.1 + 0.2"      # 3/10

Values that are genuinely irrational (like `sin(1)`) print as a decimal
approximation instead.

## Symbolic math

Any variable left undefined stays symbolic, and the result is simplified
and printed as an expression:

    calculator calculate "x + x"          # 2*x
    calculator calculate "x*(1/x)"        # 1

Explicit CAS operations:

- Differentiate — `diff(<expression>, <variable>)`:

      calculator calculate "diff(x^2, x)"          # 2*x
      calculator calculate "diff(sin(x^2), x)"     # 2*x*cos(x^2)

- Expand — `expand(<expression>)`:

      calculator calculate "expand((x+1)^2)"       # x^2 + 2*x + 1
      calculator calculate "expand((x+1)*(x-1))"   # x^2 - 1

- Factor a one-variable polynomial — `factor(<expression>)`:

      calculator calculate "factor(x^2 - 1)"       # (x - 1)*(x + 1)
      calculator calculate "factor(6*x + 9)"       # 3*(2*x + 3)

- Solve an equation for a variable — `solve(<lhs> = <rhs>, <variable>)`
  (the `= <rhs>` is optional and defaults to `= 0`). Linear and quadratic
  equations are solved exactly, including symbolic coefficients:

      calculator calculate "solve(x^2 = 4, x)"       # x = -2, x = 2
      calculator calculate "solve(x^2 = 2, x)"       # x = -sqrt(2), x = sqrt(2)
      calculator calculate "solve(a*x + b = 0, x)"   # x = -b/a

  It reports `no solution`, `no real solutions`, or `all real numbers` when
  appropriate.

These compose: `diff`, `expand`, `factor`, and `solve` can be nested inside
each other and inside ordinary arithmetic.

## Interactive REPL

Running `calculator` with no input starts an interactive REPL. It supports
everything above, plus variable bindings that persist across lines:

    let x = 5
    diff(x^2, x)      # 2*x  (the differentiation variable is never a binding)

Type `/help` in the REPL to list its commands.
|}
