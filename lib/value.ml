open Base

(* The calculator's number type: exact rationals (zarith) with float
   fallback for transcendental results. *)
type t =
  | Exact of Q.t
  | Approx of float

let to_float (value : t) : float =
  match value with
  | Exact q -> Q.to_float q
  | Approx f -> f

(* Contagion rule: exact with exact stays exact; as soon as an approx is
   involved, the whole result is approx. *)
let lift ~(exact_op : Q.t -> Q.t -> Q.t)
    ~(float_op : float -> float -> float) (left : t) (right : t) : t =
  match (left, right) with
  | Exact a, Exact b -> Exact (exact_op a b)
  | _ -> Approx (float_op (to_float left) (to_float right))

let add (left : t) (right : t) : t =
  lift ~exact_op:Q.add ~float_op:( +. ) left right

let sub (left : t) (right : t) : t =
  lift ~exact_op:Q.sub ~float_op:( -. ) left right

let mul (left : t) (right : t) : t =
  lift ~exact_op:Q.mul ~float_op:( *. ) left right

let is_zero (value : t) : bool =
  match value with
  | Exact q -> Q.equal q Q.zero
  | Approx f -> Float.equal f 0.0

let div (left : t) (right : t) : t =
  if is_zero right then raise (Calc_error.Calc_error Division_by_zero)
  else lift ~exact_op:Q.div ~float_op:( /. ) left right

let neg (value : t) : t =
  match value with
  | Exact q -> Exact (Q.neg q)
  | Approx f -> Approx (Float.neg f)

(* Largest |exponent| still exponentiated exactly. Beyond this the result
   would be a bignum with megabytes of digits, so fall back to floats. *)
let max_exact_exponent : int = 1_048_576

(* base^e for a non-negative machine-int exponent, done on numerator and
   denominator separately so the result stays a reduced rational. *)
let pow_q (base : Q.t) (e : int) : Q.t =
  Q.make (Z.pow (Q.num base) e) (Z.pow (Q.den base) e)

let pow (left : t) (right : t) : t =
  match (left, right) with
  | Exact base, Exact expo
    when Z.equal (Q.den expo) Z.one
         && Z.fits_int (Q.num expo)
         && Int.abs (Z.to_int (Q.num expo)) <= max_exact_exponent ->
      let e = Z.to_int (Q.num expo) in
      if e >= 0 then Exact (pow_q base e)
      else if Q.equal base Q.zero then
        raise (Calc_error.Calc_error Division_by_zero)
      else Exact (pow_q (Q.inv base) (-e))
  | _ -> Approx (Float.( ** ) (to_float left) (to_float right))

(* floor/ceil of a rational via integer division of num by den. *)
let floor_q (q : Q.t) : Q.t = Q.of_bigint (Z.fdiv (Q.num q) (Q.den q))
let ceil_q (q : Q.t) : Q.t = Q.of_bigint (Z.cdiv (Q.num q) (Q.den q))

(* Round half away from zero, matching Float.round_nearest. *)
let round_q (q : Q.t) : Q.t =
  let half = Q.make Z.one (Z.of_int 2) in
  if Q.sign q >= 0 then floor_q (Q.add q half) else ceil_q (Q.sub q half)

let float_fn (name : string) : float -> float =
  match name with
  | "sin" -> Float.sin
  | "cos" -> Float.cos
  | "tan" -> Float.tan
  | "log" -> Float.log10
  | "ln" -> Float.log
  | "log2" -> fun x -> Float.log x /. Float.log 2.0
  | "exp" -> Float.exp
  | "abs" -> Float.abs
  | "floor" -> Float.round_down
  | "ceil" -> Float.round_up
  | "round" -> Float.round_nearest
  | "asin" -> Float.asin
  | "acos" -> Float.acos
  | "atan" -> Float.atan
  | "sinh" -> Float.sinh
  | "cosh" -> Float.cosh
  | "tanh" -> Float.tanh
  | "sqrt" -> Float.sqrt
  | _ -> raise (Calc_error.Calc_error (Unknown_function name))

let apply_float_fn (name : string) (value : t) : t =
  match (name, value) with
  | "abs", Exact q -> Exact (Q.abs q)
  | "floor", Exact q -> Exact (floor_q q)
  | "ceil", Exact q -> Exact (ceil_q q)
  | "round", Exact q -> Exact (round_q q)
  | "sqrt", Exact q when Q.sign q >= 0 -> (
      let num_root, num_rem = Z.sqrt_rem (Q.num q) in
      let den_root, den_rem = Z.sqrt_rem (Q.den q) in
      if Z.equal num_rem Z.zero && Z.equal den_rem Z.zero then
        Exact (Q.make num_root den_root)
      else Approx (Float.sqrt (Q.to_float q)))
  | _ -> Approx ((float_fn name) (to_float value))

let to_string (value : t) : string =
  match value with
  | Exact q ->
      if Z.equal (Q.den q) Z.one then Z.to_string (Q.num q)
      else Q.to_string q
  | Approx f -> Printf.sprintf "%g" f
