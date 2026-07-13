(** Factoring univariate polynomials over the rationals. *)

(** [factor tree] rewrites a one-variable polynomial as
    content * (linear factors) * irreducible rest, e.g.
    [x^3 - x] becomes [x*(x - 1)*(x + 1)] and [6*x + 9] becomes
    [3*(2*x + 3)]. Roots are found with the rational root theorem and
    peeled off by exact division, so the result is always mathematically
    equal to the input; anything irreducible over the rationals (like
    [x^2 + 1]) comes back unchanged.
    @raise Calc_error.Calc_error [Not_a_polynomial] when [tree] mentions
    several variables, functions, or non-integer exponents. *)
val factor : Ast.expr -> Ast.expr
