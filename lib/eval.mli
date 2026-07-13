(** Evaluation of expressions, from raw text to a result. *)
open Base
open Ast

(** [eval ~env tree] computes the value of [tree], looking up variables in
    [env]. Exact subexpressions stay exact; float-backed functions produce
    approximations (see {!Value}).
    @raise Calc_error.Calc_error if [tree] references a variable not bound
    in [env], divides by zero, or applies an unknown function. *)
val eval : env:Value.t Map.M(String).t -> expr -> Value.t

(** [evaluate ~env ~input] lexes, parses, and evaluates [input] under the
    variable bindings in [env]. Expressions are simplified symbolically:
    when every variable has a numeric value the result is a number
    ([2 + 3] is [5]), otherwise it is a pretty-printed expression
    ([x + x] is [2*x], [diff(x^2, x)] is [2*x]). Returns [Ok (text, env)]
    with the result rendered as a string and the (possibly updated)
    environment, or [Error code] with a supported {!Calc_error.error}
    describing what went wrong. Never raises. *)
val evaluate : env:Value.t Map.M(String).t -> input:string -> (string * Value.t Map.M(String).t, Calc_error.error) Result.t
