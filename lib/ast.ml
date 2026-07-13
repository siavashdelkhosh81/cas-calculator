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
