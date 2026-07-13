open Ast
open Base

(* Internal flat n-ary representation. Sub/Div/Neg are sugar and disappear on
   the way in: a - b becomes a + (-1)*b, a / b becomes a * b^-1, so no rewrite
   rule ever has to mention them. *)
type sexpr =
  | SNum of Q.t
  | SVar of string
  | SSum of sexpr list
  | SProd of sexpr list
  | SPow of sexpr * sexpr
  | SFunc of string * sexpr


let rec to_sxpr (expr: expr) : sexpr =
  match expr with
  | Num number -> SNum number
  | Var variable -> SVar variable
  | Add (left, right) -> SSum [to_sxpr left; to_sxpr right]
  | Mul (left, right) -> SProd [to_sxpr left; to_sxpr right]
  | Expo (left, right) -> SPow (to_sxpr left, to_sxpr right)
  | Func (name, expr) -> SFunc (name, to_sxpr expr)
  | Sub (left, right) ->
    SSum [to_sxpr left; SProd[SNum (Q.of_int(-1)); to_sxpr right]]
  | Div (numerator, denominator) ->
    SProd [to_sxpr numerator; SProd[SPow(to_sxpr denominator, SNum (Q.of_int (-1)))]]
  | Neg inner ->
    SProd [SNum (Q.of_int(-1)); to_sxpr inner]
  (* A diff node is resolved right here, so simplifying an expression also
     computes any derivatives it contains (innermost first, via Diff.diff's
     own recursion). *)
  | Diff (body, v) -> to_sxpr (Diff.diff body v)


(* Canonical total order: constants first, then variables, then by node kind,
   then recursively by children. The exact choice matters less than using the
   same order everywhere — grouping and equality both depend on it. *)
let kind_rank (tree : sexpr) : int =
  match tree with
  | SNum _ -> 0
  | SVar _ -> 1
  | SFunc _ -> 2
  | SPow _ -> 3
  | SProd _ -> 4
  | SSum _ -> 5

let rec compare_sexpr (left : sexpr) (right : sexpr) : int =
  match (left, right) with
  | SNum a, SNum b -> Q.compare a b
  | SVar a, SVar b -> String.compare a b
  | SFunc (name_a, arg_a), SFunc (name_b, arg_b) ->
      let by_name = String.compare name_a name_b in
      if by_name <> 0 then by_name else compare_sexpr arg_a arg_b
  | SPow (base_a, expo_a), SPow (base_b, expo_b) ->
      let by_base = compare_sexpr base_a base_b in
      if by_base <> 0 then by_base else compare_sexpr expo_a expo_b
  | SProd a, SProd b | SSum a, SSum b -> List.compare compare_sexpr a b
  | _ -> Int.compare (kind_rank left) (kind_rank right)

let equal_sexpr (left : sexpr) (right : sexpr) : bool =
  compare_sexpr left right = 0

(* A one-element sum/product is just that element. *)
let sum_of (terms : sexpr list) : sexpr =
  match terms with [] -> SNum Q.zero | [ only ] -> only | _ -> SSum terms

let prod_of (factors : sexpr list) : sexpr =
  match factors with [] -> SNum Q.one | [ only ] -> only | _ -> SProd factors

(* Fold [value] into the assoc-list group for [key] (compared canonically). *)
let add_to_group ~(key : sexpr) ~(value : 'a) ~(combine : 'a -> 'a -> 'a)
    (groups : (sexpr * 'a) list) : (sexpr * 'a) list =
  match List.Assoc.find groups key ~equal:equal_sexpr with
  | None -> (key, value) :: groups
  | Some existing ->
      List.Assoc.add groups key (combine existing value) ~equal:equal_sexpr

(* One bottom-up pass of the rewrite rules. Run to fixpoint by [simplify]. *)
let rec simplify_sexpr (tree : sexpr) : sexpr =
  match tree with
  | SNum _ | SVar _ -> tree
  | SFunc (name, arg) -> simplify_func name (simplify_sexpr arg)
  | SPow (base, expo) -> simplify_pow (simplify_sexpr base) (simplify_sexpr expo)
  | SSum children -> simplify_sum (List.map children ~f:simplify_sexpr)
  | SProd children -> simplify_prod (List.map children ~f:simplify_sexpr)

(* Collect like terms: flatten nested sums, fold constants exactly, split each
   term into (coefficient, rest) and add coefficients of equal rests. *)
and simplify_sum (children : sexpr list) : sexpr =
  let flat : sexpr list =
    List.concat_map children ~f:(fun child ->
        match child with
        | SSum inner -> inner
        (* A constant times a sum distributes when it is a *term of a sum*,
           so that (x + 1) - (x + 1) can cancel to 0. Standalone products
           keep their shape — factor's 3*(2*x + 3) must survive. *)
        | SProd [ SNum c; SSum inner ] ->
            List.map inner ~f:(fun term -> simplify_prod [ SNum c; term ])
        | other -> [ other ])
  in
  let coeff_and_rest (term : sexpr) : Q.t * sexpr option =
    match term with
    | SNum q -> (q, None)
    | SProd (SNum coeff :: rest) -> (coeff, Some (prod_of rest))
    | other -> (Q.one, Some other)
  in
  let constant, groups =
    List.fold flat ~init:(Q.zero, [])
      ~f:(fun (constant, groups) term ->
        match coeff_and_rest term with
        | coeff, None -> (Q.add constant coeff, groups)
        | coeff, Some rest ->
            (constant, add_to_group groups ~key:rest ~value:coeff ~combine:Q.add))
  in
  let terms =
    List.filter_map groups ~f:(fun (rest, coeff) ->
        if Q.equal coeff Q.zero then None
        else if Q.equal coeff Q.one then Some rest
        else Some (simplify_prod [ SNum coeff; rest ]))
  in
  let terms =
    if Q.equal constant Q.zero then terms else SNum constant :: terms
  in
  sum_of (List.sort terms ~compare:compare_sexpr)

(* Collect like factors: flatten nested products, fold constants exactly
   (0 annihilates), split each factor into (base, exponent) and add exponents
   of equal bases. Note x/x -> 1 and x^0 -> 1 assume x is nonzero, the same
   default assumption SymPy makes. *)
and simplify_prod (children : sexpr list) : sexpr =
  let flat : sexpr list =
    List.concat_map children ~f:(fun child ->
        match child with SProd inner -> inner | other -> [ other ])
  in
  let numbers, symbolic =
    List.partition_map flat ~f:(fun factor ->
        match factor with SNum q -> First q | other -> Second other)
  in
  let constant = List.fold numbers ~init:Q.one ~f:Q.mul in
  if Q.equal constant Q.zero then SNum Q.zero
  else
    let base_and_expo (factor : sexpr) : sexpr * sexpr =
      match factor with
      | SPow (base, expo) -> (base, expo)
      | other -> (other, SNum Q.one)
    in
    let groups =
      List.fold symbolic ~init:[] ~f:(fun groups factor ->
          let base, expo = base_and_expo factor in
          add_to_group groups ~key:base ~value:[ expo ]
            ~combine:(fun old_exps new_exps -> old_exps @ new_exps))
    in
    let rebuilt =
      List.map groups ~f:(fun (base, expos) ->
          simplify_pow base (simplify_sum expos))
    in
    (* Rebuilding can fold new constants out (x * 1/x -> x^0 -> 1), so
       merge them into the coefficient before assembling the result. *)
    let more_numbers, factors =
      List.partition_map rebuilt ~f:(fun factor ->
          match factor with SNum q -> First q | other -> Second other)
    in
    let constant = List.fold more_numbers ~init:constant ~f:Q.mul in
    if Q.equal constant Q.zero then SNum Q.zero
    else
      let factors =
        if Q.equal constant Q.one then factors else SNum constant :: factors
      in
      prod_of (List.sort factors ~compare:compare_sexpr)

and simplify_pow (base : sexpr) (expo : sexpr) : sexpr =
  match (base, expo) with
  (* e^0 -> 1; this deliberately includes 0^0 = 1 (the combinatorics
     convention, and what most CAS pick). *)
  | _, SNum e when Q.equal e Q.zero -> SNum Q.one
  | _, SNum e when Q.equal e Q.one -> base
  | SNum b, _ when Q.equal b Q.one -> SNum Q.one
  | SNum b, SNum e when Q.equal b Q.zero && Q.sign e > 0 -> SNum Q.zero
  (* Constant ^ integer constant folds exactly via Value.pow; it returns
     Approx (and we keep the power symbolic) when the exponent is fractional
     or too large to expand. *)
  | SNum b, SNum e when Z.equal (Q.den e) Z.one -> (
      match Value.pow (Value.Exact b) (Value.Exact e) with
      | Value.Exact result -> SNum result
      | Value.Approx _ -> SPow (base, expo))
  (* (a^b)^c -> a^(b*c) only for integer c: unsound in general, since
     (x^2)^(1/2) is |x|, not x. *)
  | SPow (inner_base, inner_expo), SNum e when Z.equal (Q.den e) Z.one ->
      simplify_pow inner_base (simplify_prod [ inner_expo; SNum e ])
  | _ -> SPow (base, expo)

(* Fold functions of constants only when the result is exact: abs/floor/ceil/
   round/sqrt-of-a-perfect-square via Value, plus the well-known special
   values. Anything else (like sin 1) stays symbolic — collapsing it to a
   float would poison exactness. *)
and simplify_func (name : string) (arg : sexpr) : sexpr =
  match arg with
  | SNum q -> (
      let special : Q.t option =
        match name with
        | ("sin" | "tan" | "asin" | "atan" | "sinh" | "tanh") when Q.equal q Q.zero ->
            Some Q.zero
        | ("cos" | "cosh" | "exp") when Q.equal q Q.zero -> Some Q.one
        | ("ln" | "log" | "log2") when Q.equal q Q.one -> Some Q.zero
        | _ -> None
      in
      match special with
      | Some value -> SNum value
      | None -> (
          match Value.apply_float_fn name (Value.Exact q) with
          | Value.Exact result -> SNum result
          | Value.Approx _ -> SFunc (name, arg)))
  | _ -> SFunc (name, arg)

(* One pass is not enough in general (simplifying a child can enable a rule
   at the parent), so iterate until nothing changes. The guard turns a rule
   set that fights itself into a clear error instead of a hang. *)
let max_passes : int = 100

let run_to_fixpoint (start : sexpr) : sexpr =
  let rec loop (remaining : int) (current : sexpr) : sexpr =
    if remaining = 0 then
      failwith "Simplify: no fixpoint after 100 passes (rules are looping)"
    else
      let next = simplify_sexpr current in
      if equal_sexpr next current then current else loop (remaining - 1) next
  in
  loop max_passes start

(* Convert back to the pretty Ast: negative constants come out as Sub/Neg and
   negative powers as Div, matching what a user would write. *)
let rec from_sexpr (tree : sexpr) : expr =
  match tree with
  | SNum q -> Num q
  | SVar name -> Var name
  | SFunc (name, arg) -> Func (name, from_sexpr arg)
  | SPow (base, SNum e) when Q.sign e < 0 ->
      Div (Num Q.one, from_sexpr (simplify_pow base (SNum (Q.neg e))))
  | SPow (base, expo) -> Expo (from_sexpr base, from_sexpr expo)
  | SProd factors -> from_prod factors
  | SSum terms -> from_sum terms

and from_prod (factors : sexpr list) : expr =
  let negated, factors =
    match factors with
    | SNum c :: rest when Q.equal c (Q.of_int (-1)) -> (true, rest)
    | _ -> (false, factors)
  in
  (* Negative-exponent factors become the denominator of one Div. *)
  let inverted, direct =
    List.partition_map factors ~f:(fun factor ->
        match factor with
        | SPow (base, SNum e) when Q.sign e < 0 ->
            First (simplify_pow base (SNum (Q.neg e)))
        | other -> Second other)
  in
  let mul_all (parts : sexpr list) : expr =
    match List.map parts ~f:from_sexpr with
    | [] -> Num Q.one
    | first :: rest -> List.fold rest ~init:first ~f:(fun acc part -> Mul (acc, part))
  in
  let quotient =
    match inverted with
    | [] -> mul_all direct
    | _ -> Div (mul_all direct, mul_all inverted)
  in
  if negated then Neg quotient else quotient

and from_sum (terms : sexpr list) : expr =
  (* Display order: the constant moves last (x - 1, x^2 + 2*x + 1) — unless
     every other term is negative, where leading with it reads better
     (1 - x). Canonical order keeps constants first; this is presentation
     only. *)
  let is_negative (term : sexpr) : bool =
    match term with
    | SNum q -> Q.sign q < 0
    | SProd (SNum c :: _) -> Q.sign c < 0
    | _ -> false
  in
  let terms =
    match terms with
    | SNum c :: rest
      when (not (List.is_empty rest))
           && not (List.for_all rest ~f:is_negative) -> rest @ [ SNum c ]
    | _ -> terms
  in
  (* Split a term into its positive form and whether it was negative, so
     x + (-1)*y renders as x - y. *)
  let split (term : sexpr) : expr * bool =
    match term with
    | SNum q when Q.sign q < 0 -> (Num (Q.neg q), true)
    | SProd (SNum c :: rest) when Q.sign c < 0 ->
        let positive_coeff = Q.neg c in
        let positive =
          if Q.equal positive_coeff Q.one then prod_of rest
          else SProd (SNum positive_coeff :: rest)
        in
        (from_sexpr positive, true)
    | other -> (from_sexpr other, false)
  in
  match terms with
  | [] -> Num Q.zero
  | first :: rest ->
      let head, head_negated = split first in
      let init = if head_negated then Neg head else head in
      List.fold rest ~init ~f:(fun acc term ->
          let part, negated = split term in
          if negated then Sub (acc, part) else Add (acc, part))

let simplify (expression: expr) : expr =
  from_sexpr (run_to_fixpoint (to_sxpr expression))

(* --- Expansion --------------------------------------------------------- *)

(* Powers of sums beyond this stay unexpanded — correct, just not unfolded. *)
let max_expanded_power : int = 256

(* The terms a factor contributes to a distributed product. *)
let terms_of (tree : sexpr) : sexpr list =
  match tree with SSum terms -> terms | other -> [ other ]

(* Multiply two sums term by term (the distributive law), collecting like
   terms immediately so (x+1)^50 grows to 51 terms, never 2^50. *)
let multiply_terms (left : sexpr list) (right : sexpr list) : sexpr list =
  let products =
    List.concat_map left ~f:(fun l ->
        List.map right ~f:(fun r -> simplify_prod [ l; r ]))
  in
  terms_of (simplify_sum products)

(* Bottom-up: expand children first, then distribute at this node. Sums and
   non-sum atoms (functions, lone powers) pass through untouched. *)
let rec expand_sexpr (tree : sexpr) : sexpr =
  match tree with
  | SNum _ | SVar _ -> tree
  | SFunc (name, arg) -> SFunc (name, expand_sexpr arg)
  | SSum children -> sum_of (List.map children ~f:expand_sexpr)
  | SProd children ->
      let factors = List.map children ~f:expand_sexpr in
      sum_of
        (List.fold factors ~init:[ SNum Q.one ] ~f:(fun acc factor ->
             multiply_terms acc (terms_of factor)))
  | SPow (base, expo) -> (
      let base = expand_sexpr base in
      let expo = expand_sexpr expo in
      match (base, expo) with
      (* An integer power of a sum unfolds by repeated multiplication.
         Negative or fractional exponents stay as they are — that is the
         correct answer, not an error. *)
      | SSum _, SNum n
        when Z.equal (Q.den n) Z.one
             && Q.sign n >= 0
             && Z.leq (Q.num n) (Z.of_int max_expanded_power) ->
          let n = Z.to_int (Q.num n) in
          sum_of
            (Fn.apply_n_times ~n
               (fun acc -> multiply_terms acc (terms_of base))
               [ SNum Q.one ])
      | _ -> SPow (base, expo))

let expand (expression : expr) : expr =
  from_sexpr (run_to_fixpoint (expand_sexpr (to_sxpr expression)))
