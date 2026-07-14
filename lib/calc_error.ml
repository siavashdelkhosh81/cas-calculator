open Base

(* Supported calculator error codes. *)
type error =
  | Invalid_char of char
  | Invalid_number of string
  | Unexpected_token of string
  | Unexpected_end
  | Missing_rparen
  | Trailing_input
  | Unbound_variable of string
  | Unknown_function of string
  | Failed_to_install
  | No_ai_tool_found
  | Division_by_zero
  | Expected_comma
  | Expected_variable_name
  | Not_differentiable of string
  | Not_a_polynomial
  | Cannot_solve of string

(* Raised internally by the lexer, parser, and evaluator; caught at the
   evaluate boundary and turned into a result. *)
exception Calc_error of error

let to_string = function
  | Invalid_char c -> Printf.sprintf "invalid character: '%c'" c
  | Invalid_number text -> Printf.sprintf "invalid number: %s" text
  | Unexpected_token text -> Printf.sprintf "unexpected token: %s" text
  | Unexpected_end -> "unexpected end of input"
  | Missing_rparen -> "missing closing ')'"
  | Trailing_input -> "unexpected tokens after the expression"
  | Failed_to_install -> "unexpected error when installing skill"
  | No_ai_tool_found -> "no ai tool found"
  | Unbound_variable name -> Printf.sprintf "unbound variable: %s" name
  | Unknown_function name -> Printf.sprintf "unknown function: %s" name
  | Division_by_zero -> "division by zero"
  | Expected_comma -> "expected ',' before the variable argument"
  | Expected_variable_name -> "expected a variable name as the last argument"
  | Not_differentiable name -> Printf.sprintf "cannot differentiate '%s'" name
  | Not_a_polynomial -> "not a polynomial in one variable"
  | Cannot_solve var ->
      Printf.sprintf "cannot solve: not a polynomial equation in %s" var
