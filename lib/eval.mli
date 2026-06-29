(** Evaluation of expressions, from raw text to a result. *)

open Ast

(** [eval tree] computes the numeric value of [tree].
    @raise Error.Calc_error if [tree] references an unbound variable. *)
val eval : expr -> float

(** [evaluate input] lexes, parses, and evaluates [input]. Returns [Ok text]
    with the result rendered as a string, or [Error code] with a supported
    {!Error.error} describing what went wrong. Never raises. *)
val evaluate : string -> (string, Error.error) result
