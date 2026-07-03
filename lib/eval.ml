open Ast

(* Compute the numeric value of an expression tree. *)
let rec eval (tree : expr) : float =
  match tree with
  | Mul (left, right) -> eval left *. eval right
  | Expo (left, right) -> eval left ** eval right
  | Div (left, right) -> eval left /. eval right
  | Num value -> value
  | Add (left, right) -> eval left +. eval right
  | Sub (left, right) -> eval left -. eval right
  | Var name -> raise (Error.Calc_error (Unbound_variable name))
  | Func (name, arg) -> (
      let x = eval arg in
      match name with
      | "sin" -> sin x
      | "cos" -> cos x
      | _ -> raise (Error.Calc_error (Unknown_function name)))

(* Lex, parse, and evaluate raw input, turning any failure into a result
   carrying a supported error code. *)
let evaluate (input : string) : (string, Error.error) result =
  try
    let tokens = Lexer.tokenize input in
    let tree = Parser.parser tokens in
    Ok (Printf.sprintf "%g" (eval tree))
  with Error.Calc_error err -> Error err
