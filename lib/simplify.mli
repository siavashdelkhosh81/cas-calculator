(** Symbolic simplification and expansion of expression trees. *)
open Ast

(** [simplify tree] rewrites [tree] into a simpler canonical form: like
    terms and factors collected, constants folded exactly, identities
    ([+ 0], [* 1], [^ 1]) removed. Any [diff] nodes are resolved along the
    way. Factored shapes are respected — [3*(2*x + 3)] is not multiplied
    out. *)
val simplify : expr -> expr

(** [expand tree] additionally distributes products over sums and unfolds
    non-negative integer powers of sums, then collects like terms:
    [(x+1)^2] becomes [x^2 + 2*x + 1]. Explicit — never applied
    automatically, because neither direction is always simpler. *)
val expand : expr -> expr
