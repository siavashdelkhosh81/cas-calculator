open Ast
open Lexer

(** [parse tokens] parses one statement: a let binding
    ([let x = <expr>]), a solve request ([solve(<expr> [= <expr>], <var>)]),
    or a plain expression.
    @raise Calc_error.Calc_error on malformed input. *)
val parse : token list -> statement
