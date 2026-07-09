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
      | "log2" -> Float.log x /. Float.log 2.0
      | "exp" -> Float.exp x
      | "abs" -> Float.abs x
      | "floor" -> Float.round_down x
      | "ceil" -> Float.round_up x
      | "round" -> Float.round_nearest x
      | "asin" -> Float.asin x
      | "acos" -> Float.acos x
      | "atan" -> Float.atan x
      | "sinh" -> Float.sinh x
      | "cosh" -> Float.cosh x
      | "tanh" -> Float.tanh x
      | "sqrt" -> Float.sqrt x
      | _ -> raise (Calc_error.Calc_error (Unknown_function name)))
  | Var name -> raise (Calc_error.Calc_error (Unbound_variable name))

(* Lex, parse, and evaluate raw input, turning any failure into a result
   carrying a supported error code. *)
let evaluate (input : string) : (string, Calc_error.error) Result.t =
  try
    let tokens = Lexer.tokenize input in
    match Parser.parse tokens with
    | Let_binding (name, expr_tree) -> Ok (Printf.sprintf "%s = %g" name (eval expr_tree))
    | Experssion expr_tree ->  Ok (Printf.sprintf "%g" (eval expr_tree))
  with Calc_error.Calc_error err -> Error err
