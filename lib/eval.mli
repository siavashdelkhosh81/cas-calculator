(** Evaluation of parsed expression trees. *)

open Ast

(** [eval tree] walks the expression [tree], computes its numeric value, and
    returns it rendered as a string (the text shown after [=] in the REPL).

    @raise Failure if the expression cannot be evaluated, e.g. it references a
    variable that has no bound value. *)
val eval : expr -> string
