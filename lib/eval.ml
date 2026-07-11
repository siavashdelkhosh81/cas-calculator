open Base
open Ast

(* Compute the value of an expression tree, looking up variables in [env].
   Literals are exact; approximation only enters through the float-backed
   functions (see Value's contagion rule). *)
let rec eval ~(env : Value.t Map.M(String).t) (tree : expr) : Value.t =
  match tree with
  | Num q -> Value.Exact q
  | Mul (left, right) -> Value.mul (eval ~env left) (eval ~env right)
  | Expo (left, right) -> Value.pow (eval ~env left) (eval ~env right)
  | Div (left, right) -> Value.div (eval ~env left) (eval ~env right)
  | Add (left, right) -> Value.add (eval ~env left) (eval ~env right)
  | Sub (left, right) -> Value.sub (eval ~env left) (eval ~env right)
  | Neg exp -> Value.neg (eval ~env exp)
  | Func (name, arg) -> Value.apply_float_fn name (eval ~env arg)
  | Var name -> (
      match Map.find env name with
      | Some value -> value
      | None -> raise (Calc_error.Calc_error (Unbound_variable name)))

(* Lex, parse, and evaluate raw input, turning any failure into a result
   carrying a supported error code. *)
let evaluate ~(env : Value.t Map.M(String).t) ~(input : string)
       : (string * Value.t Map.M(String).t, Calc_error.error) Result.t =
  try
    let tokens = Lexer.tokenize input in
    match Parser.parse tokens with
    | Let_binding (name, expr_tree) ->
      let value = eval ~env expr_tree in
      let new_env = Map.set env ~key:name ~data:value in
      Ok ((Printf.sprintf "%s = %s" name (Value.to_string value)), new_env)
    | Expression expr_tree -> Ok ((Value.to_string (eval ~env expr_tree)), env)
  with Calc_error.Calc_error err -> Error err
