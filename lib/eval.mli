(** Evaluation of expressions, from raw text to a result. *)
open Base
open Ast

(** [eval ~env tree] computes the numeric value of [tree], looking up
    variables in [env].
    @raise Calc_error.Calc_error if [tree] references a variable not bound
    in [env]. *)
val eval : env:float Map.M(String).t -> expr -> float

(** [evaluate ~env ~input] lexes, parses, and evaluates [input] under the
    variable bindings in [env]. Returns [Ok (text, env)] with the result
    rendered as a string and the (possibly updated) environment, or
    [Error code] with a supported {!Calc_error.error} describing what went
    wrong. Never raises. *)
val evaluate : env:float Map.M(String).t -> input:string -> (string * float Map.M(String).t, Calc_error.error) Result.t
