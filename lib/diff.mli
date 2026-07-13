(** Symbolic differentiation. *)
open Ast

(** [diff tree v] is the derivative of [tree] with respect to the variable
    [v], by direct application of the textbook rules (sum, product, quotient,
    power, chain). Variables other than [v] are treated as constants.

    The result is correct but not tidy — pipe it through {!Simplify.simplify}
    before showing it to anyone.

    @raise Calc_error.Calc_error [Not_differentiable] for functions with no
    derivative rule ([abs], [floor], [ceil], [round]). *)
val diff : expr -> string -> expr
