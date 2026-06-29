(* The expression tree. *)
type expr =
  | Num of float
  | Var of string
  | Add of expr * expr
  | Mul of expr * expr
