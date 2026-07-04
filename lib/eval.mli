(** Evaluation of expressions, from raw text to a result. *)

open Ast

(** [eval tree] computes the numeric value of [tree].
    @raise Calc_error.Calc_error if [tree] references an unbound variable. *)
val eval : expr -> float

(** [evaluate input] lexes, parses, and evaluates [input]. Returns [Ok text]
    with the result rendered as a string, or [Error code] with a supported
    {!Calc_error.error} describing what went wrong. Never raises. *)
val evaluate : string -> (string, Calc_error.error) result
