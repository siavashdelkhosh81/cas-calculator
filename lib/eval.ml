open Base
open Ast

(* Compute the numeric value of an expression tree, looking up variables in
   [env]. *)
let rec eval ~(env : float Map.M(String).t) (tree : expr) : float =
  match tree with
  | Mul (left, right) -> eval ~env left *. eval ~env right
  | Expo (left, right) -> Float.( ** ) (eval ~env left) (eval ~env right)
  | Div (left, right) -> eval ~env left /. eval ~env right
  | Num value -> value
  | Add (left, right) -> eval ~env left +. eval ~env right
  | Sub (left, right) -> eval ~env left -. eval ~env right
  | Neg exp -> Float.neg (eval ~env exp)
  | Func (name, arg) -> (
      let x = eval ~env arg in
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
  | Var name -> (
      match Map.find env name with
      | Some value -> value
      | None -> raise (Calc_error.Calc_error (Unbound_variable name)))

(* Lex, parse, and evaluate raw input, turning any failure into a result
   carrying a supported error code. *)
let evaluate ~(env : float Map.M(String).t) ~(input : string)
       : (string * float Map.M(String).t, Calc_error.error) Result.t =
  try
    let tokens = Lexer.tokenize input in
    match Parser.parse tokens with
    | Let_binding (name, expr_tree) ->
      let value = eval ~env expr_tree in
      let new_env = Map.set env ~key:name ~data:value in
      Ok ((Printf.sprintf "%s = %g" name (value)), new_env)
    | Expression expr_tree -> Ok ((Printf.sprintf "%g" (eval ~env expr_tree)), env)
  with Calc_error.Calc_error err -> Error err
