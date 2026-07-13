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
  (* Numeric evaluation of a derivative: differentiate, then evaluate. The
     diff variable names a direction, not a binding, so it is shadowed — if
     the derivative still mentions it, that is a genuine unbound variable. *)
  | Diff (body, v) -> eval ~env:(Map.remove env v) (Diff.diff body v)

(* expand(...) and factor(...) are explicit tree transformations, resolved
   innermost-first and before simplification — so a freshly factored result
   is never collapsed back into a sum. *)
let rec apply_transforms (tree : expr) : expr =
  match tree with
  | Func ("expand", body) -> Simplify.expand (apply_transforms body)
  | Func ("factor", body) -> Factor.factor (apply_transforms body)
  | Num _ | Var _ -> tree
  | Add (a, b) -> Add (apply_transforms a, apply_transforms b)
  | Sub (a, b) -> Sub (apply_transforms a, apply_transforms b)
  | Mul (a, b) -> Mul (apply_transforms a, apply_transforms b)
  | Div (a, b) -> Div (apply_transforms a, apply_transforms b)
  | Expo (a, b) -> Expo (apply_transforms a, apply_transforms b)
  | Func (name, a) -> Func (name, apply_transforms a)
  | Neg a -> Neg (apply_transforms a)
  | Diff (body, v) -> Diff (apply_transforms body, v)

(* Every variable used as a diff(...) differentiation variable. Their env
   bindings must not leak into the result: `let x = 5` then `diff(x^2, x)`
   is 2*x, never 10. *)
let rec diff_variables (tree : expr) : Set.M(String).t =
  match tree with
  | Num _ | Var _ -> Set.empty (module String)
  | Add (a, b) | Sub (a, b) | Mul (a, b) | Div (a, b) | Expo (a, b) ->
      Set.union (diff_variables a) (diff_variables b)
  | Func (_, a) | Neg a -> diff_variables a
  | Diff (body, v) -> Set.add (diff_variables body) v

(* Replace variables bound to exact values by their numbers, so `let c = 3`
   makes diff(c*x^2, x) differentiate 3*x^2. Two exceptions stay symbolic:
   the diff variable itself (in `let x = 5` then `diff(x^2, x)` the inner x
   is the differentiation variable, not the binding — so it is shadowed),
   and approximate bindings, which have no exact tree form. *)
let rec substitute_bindings ~(env : Value.t Map.M(String).t) (tree : expr)
    : expr =
  match tree with
  | Num _ -> tree
  | Var name -> (
      match Map.find env name with
      | Some (Value.Exact q) -> Num q
      | Some (Value.Approx _) | None -> tree)
  | Diff (body, v) ->
      Diff (substitute_bindings ~env:(Map.remove env v) body, v)
  | Add (a, b) -> Add (substitute_bindings ~env a, substitute_bindings ~env b)
  | Sub (a, b) -> Sub (substitute_bindings ~env a, substitute_bindings ~env b)
  | Mul (a, b) -> Mul (substitute_bindings ~env a, substitute_bindings ~env b)
  | Div (a, b) -> Div (substitute_bindings ~env a, substitute_bindings ~env b)
  | Expo (a, b) -> Expo (substitute_bindings ~env a, substitute_bindings ~env b)
  | Func (name, a) -> Func (name, substitute_bindings ~env a)
  | Neg a -> Neg (substitute_bindings ~env a)

(* Evaluate an expression for display: substitute the bindings, simplify
   (which also computes any derivatives), then print a number when every
   remaining variable still has a usable numeric value — otherwise
   pretty-print the symbolic expression. Variables can be left over for
   three reasons: unbound, bound to an approximation (evaluated numerically
   here, not substituted), or shadowed as a diff variable (always symbolic). *)
let render_expression ~(env : Value.t Map.M(String).t) (tree : expr) : string =
  let transformed = apply_transforms (substitute_bindings ~env tree) in
  let simplified = Simplify.simplify transformed in
  let shadowed : Set.M(String).t = diff_variables tree in
  let is_numeric (name : string) : bool =
    Map.mem env name && not (Set.mem shadowed name)
  in
  if Set.for_all (free_variables simplified) ~f:is_numeric then
    Value.to_string (eval ~env simplified)
  else Printer.to_string simplified

(* Lex, parse, and evaluate raw input, turning any failure into a result
   carrying a supported error code. *)
let evaluate ~(env : Value.t Map.M(String).t) ~(input : string)
       : (string * Value.t Map.M(String).t, Calc_error.error) Result.t =
  try
    let tokens = Lexer.tokenize input in
    match Parser.parse tokens with
    | Let_binding (name, expr_tree) ->
      let value = eval ~env (apply_transforms expr_tree) in
      let new_env = Map.set env ~key:name ~data:value in
      Ok ((Printf.sprintf "%s = %s" name (Value.to_string value)), new_env)
    | Expression expr_tree -> Ok (render_expression ~env expr_tree, env)
    | Solve (left, right, variable) ->
      (* Like diff, the solve variable names an unknown, not a binding, so
         it is shadowed while other bound variables substitute in. *)
      let env_without_var = Map.remove env variable in
      let prepare (side : expr) : expr =
        apply_transforms (substitute_bindings ~env:env_without_var side)
      in
      let result =
        Solve.solve ~left:(prepare left) ~right:(prepare right) ~var:variable
      in
      Ok (Solve.to_string result ~var:variable, env)
  with Calc_error.Calc_error err -> Error err
