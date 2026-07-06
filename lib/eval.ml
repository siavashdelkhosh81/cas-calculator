open Base
open Ast

(* Compute the numeric value of an expression tree. *)
let rec eval (tree : expr) : float =
  match tree with
  | Mul (left, right) -> eval left *. eval right
  | Expo (left, right) -> Float.( ** ) (eval left) (eval right)
  | Div (left, right) -> eval left /. eval right
  | Num value -> value
  | Add (left, right) -> eval left +. eval right
  | Sub (left, right) -> eval left -. eval right
  | Neg exp -> Float.neg (eval exp)
  | Func (name, arg) -> (
      let x = eval arg in
      match name with
      | "sin" -> Float.sin x
      | "cos" -> Float.cos x
      | "tan" -> Float.tan x
      | "log" -> Float.log10 x
      | "ln" -> Float.log x
      | "sqrt" -> Float.sqrt x
      | _ -> raise (Calc_error.Calc_error (Unknown_function name)))
  | Var name -> raise (Calc_error.Calc_error (Unbound_variable name))

(* Lex, parse, and evaluate raw input, turning any failure into a result
   carrying a supported error code. *)
let evaluate (input : string) : (string, Calc_error.error) Result.t =
  try
    let tokens = Lexer.tokenize input in
    let tree = Parser.parser tokens in
    Ok (Printf.sprintf "%g" (eval tree))
  with Calc_error.Calc_error err -> Error err
