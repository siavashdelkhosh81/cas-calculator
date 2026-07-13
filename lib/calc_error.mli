(** Calculator error codes and helpers. *)

(** Every way evaluation can fail. *)
type error =
  | Invalid_char of char        (** lexer hit a character it doesn't know *)
  | Invalid_number of string    (** a numeric literal that won't parse, e.g. ["1.2.3"] *)
  | Unexpected_token of string  (** parser found a token it didn't expect here *)
  | Unexpected_end              (** input ended while more was expected *)
  | Missing_rparen              (** an opening ['('] had no matching [')'] *)
  | Trailing_input              (** extra tokens left after a complete expression *)
  | Unbound_variable of string  (** evaluator met a variable with no value *)
  | Unknown_function of string  (** a function name the evaluator doesn't implement *)
  | Failed_to_install           (** failed to install the skill *)
  | No_ai_tool_found            (** no supported AI tool directory in home *)
  | Division_by_zero            (** divisor evaluated to zero *)
  | Expected_comma              (** [diff] needs two comma-separated arguments *)
  | Expected_variable_name      (** the second argument of [diff] must be a variable *)
  | Not_differentiable of string(** function with no derivative rule, e.g. [abs] *)
  | Not_a_polynomial            (** [factor] needs a one-variable polynomial *)

(** Raised internally by the lexer, parser, and evaluator. It is caught at the
    {!Eval.evaluate} boundary, so callers normally see a [result] instead. *)
exception Calc_error of error

(** [to_string err] renders [err] as a human-readable message. *)
val to_string : error -> string
