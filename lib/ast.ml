(* The expression tree. *)
type expr =
  | Num of float
  | Var of string
  | Add of expr * expr
  | Mul of expr * expr

(* Render an expression tree back to a parenthesised string. *)
let rec string_of_expr = function
  | Num number -> Printf.sprintf "%g" number
  | Var name -> name
  | Add (left, right) ->
      Printf.sprintf "(%s + %s)" (string_of_expr left) (string_of_expr right)
  | Mul (left, right) ->
      Printf.sprintf "(%s * %s)" (string_of_expr left) (string_of_expr right)
