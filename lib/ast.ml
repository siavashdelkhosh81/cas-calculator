open Base

(* The expression tree. A literal in source text is always exact, so [Num]
   holds a rational; approximation only appears at evaluation time. *)
type expr =
  | Num of Q.t
  | Var of string
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Expo of expr * expr
  | Func of string * expr
  | Neg of expr
  | Diff of expr * string  (* diff(expression, variable): stays in the tree
                              so derivatives nest and compose like any other
                              expression *)


type statement =
  | Expression of expr
  | Let_binding of string * expr
  | Solve of expr * expr * string  (* solve(left = right, variable). A
                                      statement, not an expression: its
                                      result is a set of solutions, not a
                                      value, so it cannot nest. *)

(* Every variable name the tree mentions (the diff variable itself is a
   binder, not a mention, but the body underneath it still counts). *)
let rec free_variables (tree : expr) : Set.M(String).t =
  match tree with
  | Num _ -> Set.empty (module String)
  | Var name -> Set.singleton (module String) name
  | Add (a, b) | Sub (a, b) | Mul (a, b) | Div (a, b) | Expo (a, b) ->
      Set.union (free_variables a) (free_variables b)
  | Func (_, a) | Neg a -> free_variables a
  | Diff (body, _) -> free_variables body
