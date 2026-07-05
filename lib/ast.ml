(* The expression tree. *)
type expr =
  | Num of float
  | Var of string
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Expo of expr * expr
  | Func of string * expr
  | Neg of expr
