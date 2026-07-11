(** The calculator's number type: exact rationals with float fallback. *)

type t =
  | Exact of Q.t  (** fractions and integers of any size *)
  | Approx of float  (** result of sin, cos, ln, non-square sqrt, ... *)

(** Arithmetic follows the contagion rule: combining two [Exact] values is
    exact; as soon as an [Approx] is involved, the result is [Approx]. *)

val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> t -> t

(** @raise Calc_error.Calc_error [Division_by_zero] when the divisor is an
    exact zero or an approx 0.0. *)
val div : t -> t -> t

val neg : t -> t

(** [pow base exponent] stays exact when [base] is exact and [exponent] is an
    exact integer of reasonable size (negative exponent = power of the
    inverse); otherwise falls back to float exponentiation.
    @raise Calc_error.Calc_error [Division_by_zero] on [0 ^ negative]. *)
val pow : t -> t -> t

(** [apply_float_fn name v] applies the named unary function. [abs], [floor],
    [ceil], [round] preserve exactness, and [sqrt] of an exact perfect square
    stays exact; everything else converts to float and returns [Approx].
    @raise Calc_error.Calc_error [Unknown_function] for unsupported names. *)
val apply_float_fn : string -> t -> t

val to_float : t -> float

(** Exact integers print bare ([4]), other rationals as [num/den] ([1/2]),
    approx values with [%g]. *)
val to_string : t -> string
