open Base
open Ast

(* Precedence levels, higher binds tighter. A child is parenthesized when its
   level is below the level its position requires. *)
let add_level : int = 1
let mul_level : int = 2
let pow_level : int = 3
let atom_level : int = 4

(* A numeral is not always an atom: "-3" pasted into "x^-3" or "1/2" into
   "x*1/2" would re-parse differently, so negative and fractional constants
   claim the precedence of the operator their text contains. *)
let num_level (q : Q.t) : int =
  if Q.sign q < 0 then add_level
  else if not (Z.equal (Q.den q) Z.one) then mul_level
  else atom_level

(* [render tree ~context] is [tree] as text, wrapped in parentheses when the
   surrounding [context] binds tighter than the tree's own operator. *)
let rec render (tree : expr) ~(context : int) : string =
  let text, level =
    match tree with
    | Num q -> (Q.to_string q, num_level q)
    | Var name -> (name, atom_level)
    | Add (a, b) ->
        (render a ~context:add_level ^ " + " ^ render b ~context:add_level,
         add_level)
    (* Subtraction is left-associative, so the right side needs one level
       more: x - (y + z) keeps its parentheses. *)
    | Sub (a, b) ->
        (render a ~context:add_level ^ " - " ^ render b ~context:(add_level + 1),
         add_level)
    | Mul (a, b) ->
        (render a ~context:mul_level ^ "*" ^ render b ~context:mul_level,
         mul_level)
    | Div (a, b) ->
        (render a ~context:mul_level ^ "/" ^ render b ~context:(mul_level + 1),
         mul_level)
    (* Exponentiation is right-associative: (x^2)^3 needs parentheses on the
       left, x^2^3 does not need them on the right. *)
    | Expo (base, expo) ->
        (render base ~context:(pow_level + 1) ^ "^" ^ render expo ~context:pow_level,
         pow_level)
    | Neg a -> ("-" ^ render a ~context:mul_level, add_level)
    | Func (name, arg) ->
        (name ^ "(" ^ render arg ~context:add_level ^ ")", atom_level)
    | Diff (body, v) ->
        ("diff(" ^ render body ~context:add_level ^ ", " ^ v ^ ")", atom_level)
  in
  if level < context then "(" ^ text ^ ")" else text

let to_string (tree : expr) : string = render tree ~context:add_level
