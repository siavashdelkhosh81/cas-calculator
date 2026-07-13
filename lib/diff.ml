open Base
open Ast

(* Shorthands so the rules below read like the textbook table. *)
let num (n : int) : expr = Num (Q.of_int n)
let ln (e : expr) : expr = Func ("ln", e)
let squared (e : expr) : expr = Expo (e, num 2)

(* [diff tree v] is the derivative of [tree] with respect to [v]: one match
   arm per textbook rule. Every function rule already carries its chain-rule
   factor (the [* u'] part), so nested expressions work by plain recursion.

   The trees built here are deliberately sloppy — [1*x^1], [+ 0] and friends
   are all fine, because Simplify cleans them up afterwards. diff's only job
   is to be correct; simplify's job is to be pretty. *)
let rec diff (tree : expr) (v : string) : expr =
  match tree with
  (* Constants and variables. *)
  | Num _ -> Num Q.zero
  | Var name -> if String.equal name v then Num Q.one else Num Q.zero
  (* Linearity. *)
  | Add (a, b) -> Add (diff a v, diff b v)
  | Sub (a, b) -> Sub (diff a v, diff b v)
  | Neg a -> Neg (diff a v)
  (* Product rule: (u*w)' = u'*w + u*w'. *)
  | Mul (a, b) -> Add (Mul (diff a v, b), Mul (a, diff b v))
  (* Quotient rule: (u/w)' = (u'*w - u*w') / w^2. *)
  | Div (a, b) ->
      Div (Sub (Mul (diff a v, b), Mul (a, diff b v)), squared b)
  (* Power rule + chain: (u^c)' = c * u^(c-1) * u'. The exponent arithmetic
     is exact, so x^(1/2) differentiates to (1/2) * x^(-1/2). *)
  | Expo (base, Num c) ->
      Mul (Mul (Num c, Expo (base, Num (Q.sub c Q.one))), diff base v)
  (* Exponential rule: (c^u)' = c^u * ln(c) * u'. *)
  | Expo (Num c, expo) ->
      Mul (Mul (Expo (Num c, expo), ln (Num c)), diff expo v)
  (* General case, both sides non-constant:
     (u^w)' = u^w * (w' * ln(u) + w * u'/u). *)
  | Expo (base, expo) ->
      Mul
        ( Expo (base, expo),
          Add
            ( Mul (diff expo v, ln base),
              Mul (expo, Div (diff base v, base)) ) )
  | Func (name, u) -> diff_func name u v
  (* A nested diff is just the derivative of the inner derivative. *)
  | Diff (inner, w) -> diff (diff inner w) v

(* The one-argument functions. Each arm is d/dv f(u) = f'(u) * u'. *)
and diff_func (name : string) (u : expr) (v : string) : expr =
  let u' : expr = diff u v in
  match name with
  | "sin" -> Mul (Func ("cos", u), u')
  | "cos" -> Neg (Mul (Func ("sin", u), u'))
  | "tan" -> Div (u', squared (Func ("cos", u)))
  | "exp" -> Mul (Func ("exp", u), u')
  | "ln" -> Div (u', u)
  | "log" -> Div (u', Mul (u, ln (num 10)))
  | "log2" -> Div (u', Mul (u, ln (num 2)))
  | "sqrt" -> Div (u', Mul (num 2, Func ("sqrt", u)))
  | "asin" -> Div (u', Func ("sqrt", Sub (num 1, squared u)))
  | "acos" -> Neg (Div (u', Func ("sqrt", Sub (num 1, squared u))))
  | "atan" -> Div (u', Add (num 1, squared u))
  | "sinh" -> Mul (Func ("cosh", u), u')
  | "cosh" -> Mul (Func ("sinh", u), u')
  | "tanh" -> Div (u', squared (Func ("cosh", u)))
  | other -> raise (Calc_error.Calc_error (Not_differentiable other))
