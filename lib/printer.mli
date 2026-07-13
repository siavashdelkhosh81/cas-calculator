(** Rendering expression trees back into calculator syntax. *)
open Ast

(** [to_string tree] renders [tree] with the fewest parentheses that still
    re-parse to the same tree: [2*x], [2*x*cos(x^2)], [1/x^2]. *)
val to_string : expr -> string
