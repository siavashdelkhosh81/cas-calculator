open Base
open Ast

type t = Q.t array

let not_a_polynomial () = raise (Calc_error.Calc_error Not_a_polynomial)

(* Strip trailing zero coefficients so every polynomial has exactly one
   representation; the zero polynomial is the empty array. *)
let normalize (coeffs : Q.t array) : t =
  let length = ref (Array.length coeffs) in
  while !length > 0 && Q.equal coeffs.(!length - 1) Q.zero do
    Int.decr length
  done;
  Array.sub coeffs ~pos:0 ~len:!length

let zero : t = [||]
let is_zero (p : t) : bool = Array.is_empty p
let degree (p : t) : int = Array.length p - 1
let constant (q : Q.t) : t = normalize [| q |]

(* The coefficient of x^i, zero beyond the stored length. *)
let coeff (p : t) (i : int) : Q.t =
  if i < Array.length p then p.(i) else Q.zero

(* c * x^k as a polynomial. *)
let monomial (c : Q.t) (k : int) : t =
  if Q.equal c Q.zero then zero
  else Array.init (k + 1) ~f:(fun i -> if i = k then c else Q.zero)

let add (a : t) (b : t) : t =
  let length = Int.max (Array.length a) (Array.length b) in
  normalize (Array.init length ~f:(fun i -> Q.add (coeff a i) (coeff b i)))

let scale (factor : Q.t) (p : t) : t =
  normalize (Array.map p ~f:(Q.mul factor))

let sub (a : t) (b : t) : t = add a (scale (Q.of_int (-1)) b)

let mul (a : t) (b : t) : t =
  if is_zero a || is_zero b then zero
  else begin
    let result =
      Array.create ~len:(Array.length a + Array.length b - 1) Q.zero
    in
    Array.iteri a ~f:(fun i ai ->
        Array.iteri b ~f:(fun j bj ->
            result.(i + j) <- Q.add result.(i + j) (Q.mul ai bj)));
    normalize result
  end

(* Long division: dividend = quotient * divisor + remainder, with the
   remainder's degree below the divisor's. Each step cancels the leading
   term exactly (rational arithmetic), so the loop always terminates. *)
let divmod (dividend : t) (divisor : t) : t * t =
  if is_zero divisor then raise (Calc_error.Calc_error Division_by_zero);
  let divisor_degree = degree divisor in
  let lead = divisor.(divisor_degree) in
  if degree dividend < divisor_degree then (zero, dividend)
  else begin
    let quotient =
      Array.create ~len:(degree dividend - divisor_degree + 1) Q.zero
    in
    let remainder = ref dividend in
    while degree !remainder >= divisor_degree do
      let k = degree !remainder - divisor_degree in
      let factor = Q.div (coeff !remainder (degree !remainder)) lead in
      quotient.(k) <- factor;
      remainder := sub !remainder (mul (monomial factor k) divisor)
    done;
    (normalize quotient, !remainder)
  end

(* Horner's method: c0 + x*(c1 + x*(c2 + ...)). *)
let eval_at (p : t) (x : Q.t) : Q.t =
  Array.fold_right p ~init:Q.zero ~f:(fun c acc -> Q.add c (Q.mul acc x))

(* Structural reading of a tree as a polynomial in [var]: polynomial
   arithmetic on the pieces does any expansion for free. Anything outside
   +, -, *, /constant and non-negative integer powers is not a polynomial. *)
let rec of_expr (tree : expr) ~(var : string) : t =
  match tree with
  | Num q -> constant q
  | Var name ->
      if String.equal name var then [| Q.zero; Q.one |]
      else not_a_polynomial ()
  | Add (a, b) -> add (of_expr a ~var) (of_expr b ~var)
  | Sub (a, b) -> sub (of_expr a ~var) (of_expr b ~var)
  | Neg a -> scale (Q.of_int (-1)) (of_expr a ~var)
  | Mul (a, b) -> mul (of_expr a ~var) (of_expr b ~var)
  | Div (a, b) ->
      (* Dividing by a nonzero constant scales; anything else (1/x, ...)
         leaves polynomial land. *)
      let denominator = of_expr b ~var in
      if degree denominator = 0 then
        scale (Q.inv (coeff denominator 0)) (of_expr a ~var)
      else not_a_polynomial ()
  | Expo (base, Num n)
    when Z.equal (Q.den n) Z.one
         && Q.sign n >= 0
         && Z.leq (Q.num n) (Z.of_int 10_000) ->
      let base = of_expr base ~var in
      Fn.apply_n_times ~n:(Z.to_int (Q.num n)) (mul base) (constant Q.one)
  | Expo _ | Func _ | Diff _ -> not_a_polynomial ()

let to_expr (p : t) ~(var : string) : expr =
  (* Sloppy c*x^i sum — callers pipe the result through Simplify. *)
  if is_zero p then Num Q.zero
  else
    Array.foldi p ~init:(Num Q.zero) ~f:(fun i acc c ->
        Add (acc, Mul (Num c, Expo (Var var, Num (Q.of_int i)))))
