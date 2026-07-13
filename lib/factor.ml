open Base
open Ast

(* Numbers whose divisors we refuse to enumerate — past this, factoring
   gives up gracefully and returns what it has, rather than hang. *)
let divisor_search_limit : int = 1_000_000_000_000

(* All positive divisors of [n], by trial division up to sqrt(n); [None]
   when [n] is too large to enumerate. *)
let divisors (n : Z.t) : Z.t list option =
  let n = Z.abs n in
  if not (Z.fits_int n) || Z.gt n (Z.of_int divisor_search_limit) then None
  else begin
    let n = Z.to_int n in
    let found = ref [] in
    let d = ref 1 in
    while !d * !d <= n do
      if n % !d = 0 then begin
        found := Z.of_int !d :: !found;
        if !d <> n / !d then found := Z.of_int (n / !d) :: !found
      end;
      Int.incr d
    done;
    Some !found
  end

(* Rational root theorem: any rational root p/q (in lowest terms) of an
   integer polynomial has p dividing the constant term and q dividing the
   leading coefficient. Root 0 is the constant-term-is-zero case. *)
let find_rational_root (p : Polynomial.t) : Q.t option =
  if Q.equal (Polynomial.coeff p 0) Q.zero then Some Q.zero
  else
    let constant_divisors = divisors (Q.num (Polynomial.coeff p 0)) in
    let leading_divisors =
      divisors (Q.num (Polynomial.coeff p (Polynomial.degree p)))
    in
    match (constant_divisors, leading_divisors) with
    | Some numerators, Some denominators ->
        let candidates =
          List.concat_map numerators ~f:(fun num ->
              List.concat_map denominators ~f:(fun den ->
                  [ Q.make num den; Q.make (Z.neg num) den ]))
        in
        List.find candidates ~f:(fun r ->
            Q.equal (Polynomial.eval_at p r) Q.zero)
    | _ -> None

(* poly = content * primitive, where primitive has coprime integer
   coefficients and a positive leading one: 6x + 9 = 3 * (2x + 3). *)
let content_and_primitive (poly : Polynomial.t) : Q.t * Polynomial.t =
  let common_denominator : Z.t =
    Array.fold poly ~init:Z.one ~f:(fun acc c -> Z.lcm acc (Q.den c))
  in
  let numerator_gcd : Z.t =
    Array.fold poly ~init:Z.zero ~f:(fun acc c ->
        Z.gcd acc (Q.num (Q.mul c (Q.of_bigint common_denominator))))
  in
  let sign : Z.t =
    if Q.sign (Polynomial.coeff poly (Polynomial.degree poly)) < 0 then
      Z.minus_one
    else Z.one
  in
  let content : Q.t = Q.make (Z.mul sign numerator_gcd) common_denominator in
  (content, Polynomial.scale (Q.inv content) poly)

(* The primitive linear factor with root p/q is (q*x - p): root -1 gives
   x + 1, root 1/2 gives 2*x - 1. *)
let linear_factor_of_root (root : Q.t) : Polynomial.t =
  [| Q.of_bigint (Z.neg (Q.num root)); Q.of_bigint (Q.den root) |]

(* Peel one linear factor per found root until none divides. By Gauss's
   lemma the quotient of a primitive polynomial by a primitive linear
   factor is again primitive, so the loop invariant holds throughout. *)
let rec peel_roots (p : Polynomial.t) (found : Polynomial.t list)
    : Polynomial.t list * Polynomial.t =
  if Polynomial.degree p < 1 then (List.rev found, p)
  else
    match find_rational_root p with
    | None -> (List.rev found, p)
    | Some root ->
        let linear = linear_factor_of_root root in
        let quotient, remainder = Polynomial.divmod p linear in
        if not (Polynomial.is_zero remainder) then
          failwith "Factor: a found root did not divide the polynomial";
        peel_roots quotient (linear :: found)

(* Factor a univariate rational polynomial over the rationals:
   content * x^k * (linear factors from rational roots) * irreducible rest.
   Whatever cannot be factored is returned unchanged — never a guess. *)
let factor (tree : expr) : expr =
  let simplified : expr = Simplify.simplify tree in
  match Set.to_list (free_variables simplified) with
  | [] -> simplified
  | _ :: _ :: _ -> raise (Calc_error.Calc_error Not_a_polynomial)
  | [ variable ] ->
      let poly : Polynomial.t = Polynomial.of_expr simplified ~var:variable in
      if Polynomial.degree poly <= 0 then simplified
      else begin
        let content, primitive = content_and_primitive poly in
        let linear_factors, rest = peel_roots primitive [] in
        (* A fully peeled primitive ends in the constant 1; fold whatever
           constant is left into the numeric factor. *)
        let content, remaining_factors =
          if Polynomial.degree rest < 1 then
            (Q.mul content (Polynomial.coeff rest 0), [])
          else (content, [ rest ])
        in
        let factored : expr =
          List.fold
            (linear_factors @ remaining_factors)
            ~init:(Num content)
            ~f:(fun acc piece -> Mul (acc, Polynomial.to_expr piece ~var:variable))
        in
        (* Simplify for presentation: drops the 1* when the content is
           trivial and groups repeated factors into powers — it will not
           multiply the factors back out. *)
        Simplify.simplify factored
      end
