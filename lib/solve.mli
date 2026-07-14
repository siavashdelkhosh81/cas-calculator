(** Solving polynomial equations for a variable — the CAS capstone. *)

(** What solving can produce. Solutions stay exact: rational roots are
    numbers, irrational quadratic roots are sqrt expressions, symbolic
    linear equations give expressions like [-b/a]. *)
type solution =
  | Solutions of Ast.expr list
  | No_solution        (** nothing satisfies the equation (0 = 1) *)
  | No_real_solution   (** only complex roots (x^2 + 1 = 0) *)
  | All_reals          (** everything satisfies it (0 = 0) *)
  | Partial of Ast.expr list * Ast.expr
      (** rational roots found, plus a leftover polynomial of degree >= 3
          with no rational roots — reported honestly, never guessed *)

(** [solve ~left ~right ~var] solves [left = right] for [var]. Complete for
    linear and quadratic equations (including symbolic-coefficient linear
    ones), and for any higher degree whose irrational part is at most
    quadratic; the rest comes back as [Partial]. The caller substitutes
    bound variables first, shadowing [var].
    @raise Calc_error.Calc_error [Cannot_solve] when the equation is not a
    polynomial in [var] (and not symbolically linear in it either). *)
val solve : left:Ast.expr -> right:Ast.expr -> var:string -> solution

(** [to_string result ~var] renders for the REPL: ["x = -2, x = 2"],
    ["no real solutions"], ["all real numbers"], ... *)
val to_string : solution -> var:string -> string
