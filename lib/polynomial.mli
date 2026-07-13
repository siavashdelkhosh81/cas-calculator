(** Dense univariate polynomials with exact rational coefficients — the
    workhorse behind [factor] and later CAS features. *)

(** Index [i] holds the coefficient of x^i. Always normalized: no trailing
    zeros, and the zero polynomial is the empty array. *)
type t = Q.t array

val zero : t
val is_zero : t -> bool

(** [degree p] is the highest power with a nonzero coefficient; the zero
    polynomial has degree [-1]. *)
val degree : t -> int

(** [coeff p i] is the coefficient of x^i, zero beyond the stored length. *)
val coeff : t -> int -> Q.t

val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> t -> t

(** [scale c p] multiplies every coefficient by [c]. *)
val scale : Q.t -> t -> t

(** [divmod dividend divisor] is polynomial long division: [(quotient,
    remainder)] with [degree remainder < degree divisor].
    @raise Calc_error.Calc_error [Division_by_zero] when [divisor] is the
    zero polynomial. *)
val divmod : t -> t -> t * t

(** [eval_at p x] evaluates exactly, by Horner's method. *)
val eval_at : t -> Q.t -> Q.t

(** [of_expr tree ~var] reads [tree] as a polynomial in [var].
    @raise Calc_error.Calc_error [Not_a_polynomial] on other variables,
    functions, or negative/fractional exponents. *)
val of_expr : Ast.expr -> var:string -> t

(** [to_expr p ~var] builds an unsimplified [c_i * var^i] sum — pipe it
    through {!Simplify.simplify} before showing it to anyone. *)
val to_expr : t -> var:string -> Ast.expr
