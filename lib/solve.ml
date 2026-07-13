open Base
open Ast

(* What solving an equation can produce. Distinct from an expression: an
   equation has zero, one, or many solutions — or all of them. *)
type solution =
  | Solutions of expr list  (* exact values, possibly symbolic: sqrt(2), -b/a *)
  | No_solution             (* 0 = 1: nothing satisfies the equation *)
  | No_real_solution        (* roots exist but are complex: x^2 + 1 = 0 *)
  | All_reals               (* 0 = 0: everything satisfies it *)
  | Partial of expr list * expr
      (* rational roots found, plus a leftover polynomial we cannot crack *)

let cannot_solve (var : string) : 'a =
  raise (Calc_error.Calc_error (Cannot_solve var))

(* --- Square roots, kept exact ----------------------------------------- *)

(* n = s^2 * m with the largest square pulled out: 8 is (2, 2), so
   sqrt(8) = 2*sqrt(2); a perfect square has m = 1. Numbers too large to
   trial-divide come back unreduced (correct, just not tidied). *)
let extract_square_part (n : Z.t) : Z.t * Z.t =
  let root, remainder = Z.sqrt_rem n in
  if Z.equal remainder Z.zero then (root, Z.one)
  else if not (Z.fits_int n) || Z.gt n (Z.of_int 1_000_000_000_000) then
    (Z.one, n)
  else begin
    let square = ref 1 in
    let rest = ref (Z.to_int n) in
    let f = ref 2 in
    while !f * !f <= !rest do
      while !rest % (!f * !f) = 0 do
        rest := !rest / (!f * !f);
        square := !square * !f
      done;
      Int.incr f
    done;
    (Z.of_int !square, Z.of_int !rest)
  end

(* sqrt of a positive rational as (coefficient, radicand):
   sqrt(d) = coefficient * sqrt(radicand), radicand = 1 when exact.
   Uses sqrt(p/q) = sqrt(p*q)/q to work on one integer. *)
let decompose_sqrt (d : Q.t) : Q.t * Z.t =
  let square, radicand = extract_square_part (Z.mul (Q.num d) (Q.den d)) in
  (Q.make square (Q.den d), radicand)

(* --- The quadratic formula -------------------------------------------- *)

(* a*x^2 + b*x + c = 0, rational coefficients, exact throughout. Roots come
   out as plain numbers when the discriminant is a perfect square, and as
   simplified sqrt expressions otherwise — never as floats. *)
let solve_quadratic (a : Q.t) (b : Q.t) (c : Q.t) : solution =
  let two_a = Q.mul (Q.of_int 2) a in
  let discriminant = Q.sub (Q.mul b b) (Q.mul (Q.of_int 4) (Q.mul a c)) in
  match Q.sign discriminant with
  | -1 -> No_real_solution
  | 0 -> Solutions [ Num (Q.div (Q.neg b) two_a) ]
  | _ ->
      let coefficient, radicand = decompose_sqrt discriminant in
      if Z.equal radicand Z.one then
        (* Perfect square: two exact rational roots, ascending. *)
        let root_at (sign : int) : Q.t =
          Q.div (Q.add (Q.neg b) (Q.mul (Q.of_int sign) coefficient)) two_a
        in
        let roots = List.sort [ root_at (-1); root_at 1 ] ~compare:Q.compare in
        Solutions (List.map roots ~f:(fun r -> Num r))
      else
        (* Irrational: (-b ± coefficient*sqrt(radicand)) / 2a, symbolic. *)
        let sqrt_term =
          Mul (Num coefficient, Func ("sqrt", Num (Q.of_bigint radicand)))
        in
        let root_with (combine : expr -> expr -> expr) : expr =
          Simplify.simplify
            (Div (combine (Num (Q.neg b)) sqrt_term, Num two_a))
        in
        let minus = root_with (fun l r -> Sub (l, r)) in
        let plus = root_with (fun l r -> Add (l, r)) in
        (* The minus branch is the smaller root exactly when a > 0. *)
        if Q.sign a > 0 then Solutions [ minus; plus ]
        else Solutions [ plus; minus ]

(* --- Rational polynomial equations ------------------------------------ *)

let ascending_nums (roots : Q.t list) : expr list =
  List.map (List.dedup_and_sort roots ~compare:Q.compare) ~f:(fun r -> Num r)

(* Sanity net (cheap, exact): a rational root that does not evaluate to
   zero is an internal bug, never an answer to show the user. *)
let assert_roots_hold (poly : Polynomial.t) (roots : Q.t list) : unit =
  List.iter roots ~f:(fun root ->
      if not (Q.equal (Polynomial.eval_at poly root) Q.zero) then
        failwith "Solve: produced a root that does not satisfy the equation")

let solve_polynomial (poly : Polynomial.t) ~(var : string) : solution =
  match Polynomial.degree poly with
  | -1 -> All_reals   (* the equation collapsed to 0 = 0 *)
  | 0 -> No_solution  (* ... to c = 0 with c <> 0 *)
  | 1 ->
      let root = Q.neg (Q.div (Polynomial.coeff poly 0) (Polynomial.coeff poly 1)) in
      assert_roots_hold poly [ root ];
      Solutions [ Num root ]
  | 2 ->
      solve_quadratic (Polynomial.coeff poly 2) (Polynomial.coeff poly 1)
        (Polynomial.coeff poly 0)
  | _ -> (
      let rational, rest = Factor.rational_roots poly in
      assert_roots_hold poly rational;
      match Polynomial.degree rest with
      | 0 -> Solutions (ascending_nums rational)  (* fully peeled *)
      | 2 -> (
          (* The peel found every rational root, so this quadratic can only
             have irrational or complex ones. *)
          match
            solve_quadratic (Polynomial.coeff rest 2) (Polynomial.coeff rest 1)
              (Polynomial.coeff rest 0)
          with
          | Solutions irrational ->
              Solutions (ascending_nums rational @ irrational)
          | _ when List.is_empty rational -> No_real_solution
          | _ -> Solutions (ascending_nums rational))
      | _ ->
          Partial
            (ascending_nums rational, Simplify.simplify (Polynomial.to_expr rest ~var)))

(* --- Linear equations with symbolic coefficients ----------------------- *)

let rec substitute (tree : expr) ~(var : string) ~(value : expr) : expr =
  match tree with
  | Num _ -> tree
  | Var name -> if String.equal name var then value else tree
  | Add (a, b) -> Add (substitute a ~var ~value, substitute b ~var ~value)
  | Sub (a, b) -> Sub (substitute a ~var ~value, substitute b ~var ~value)
  | Mul (a, b) -> Mul (substitute a ~var ~value, substitute b ~var ~value)
  | Div (a, b) -> Div (substitute a ~var ~value, substitute b ~var ~value)
  | Expo (a, b) -> Expo (substitute a ~var ~value, substitute b ~var ~value)
  | Func (name, a) -> Func (name, substitute a ~var ~value)
  | Neg a -> Neg (substitute a ~var ~value)
  | Diff (body, v) ->
      (* The diff variable shadows [var] inside its own body. *)
      if String.equal v var then tree
      else Diff (substitute body ~var ~value, v)

let is_zero_expr (tree : expr) : bool =
  match tree with Num q -> Q.equal q Q.zero | _ -> false

(* The equation could not be read as a rational polynomial (other variables
   or functions are present). It is still solvable if it is *linear* in the
   variable: slope*x + intercept with both pieces x-free — that is the
   `solve(a*x + b = 0, x)` → `x = -b/a` case. Anything else is out of
   scope: symbolic quadratics would need "is a zero?" case splits. *)
let solve_symbolic_linear (equation : expr) ~(var : string) : solution =
  let slope =
    try Simplify.simplify (Diff.diff equation var)
    with Calc_error.Calc_error _ -> cannot_solve var
  in
  if Set.mem (free_variables slope) var then cannot_solve var;
  let intercept =
    Simplify.simplify (substitute equation ~var ~value:(Num Q.zero))
  in
  (* The derivative trick assumes the equation really is slope*x +
     intercept — verify that exactly before answering. *)
  let reconstructed = Add (Mul (slope, Var var), intercept) in
  if not (is_zero_expr (Simplify.simplify (Sub (equation, reconstructed))))
  then cannot_solve var;
  if is_zero_expr slope then
    (* No x at all: the constant rule, on a possibly-symbolic constant. *)
    if is_zero_expr intercept then All_reals else No_solution
  else Solutions [ Simplify.simplify (Neg (Div (intercept, slope))) ]

(* --- Entry point -------------------------------------------------------- *)

(* solve(left = right, var): normalize to left - right = 0, then take the
   rational-polynomial path when possible, else the symbolic-linear one.
   Bound-variable substitution is the caller's job (Eval does it, shadowing
   [var] exactly like diff). *)
let solve ~(left : expr) ~(right : expr) ~(var : string) : solution =
  let equation = Simplify.simplify (Sub (left, right)) in
  match Polynomial.of_expr equation ~var with
  | poly -> solve_polynomial poly ~var
  | exception Calc_error.Calc_error Not_a_polynomial ->
      solve_symbolic_linear equation ~var

(* --- Presentation ------------------------------------------------------ *)

let to_string (result : solution) ~(var : string) : string =
  let show (roots : expr list) : string =
    String.concat ~sep:", "
      (List.map roots ~f:(fun root ->
           Printf.sprintf "%s = %s" var (Printer.to_string root)))
  in
  match result with
  | Solutions roots -> show roots
  | No_solution -> "no solution"
  | No_real_solution -> "no real solutions"
  | All_reals -> "all real numbers"
  | Partial ([], leftover) ->
      Printf.sprintf "unsolved: roots of %s" (Printer.to_string leftover)
  | Partial (roots, leftover) ->
      Printf.sprintf "%s; unsolved: roots of %s" (show roots)
        (Printer.to_string leftover)
