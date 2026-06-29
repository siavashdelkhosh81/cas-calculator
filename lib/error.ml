(* Supported calculator error codes. *)
type error =
  | Invalid_char of char
  | Invalid_number of string
  | Unexpected_token of string
  | Unexpected_end
  | Missing_rparen
  | Trailing_input
  | Unbound_variable of string

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
  | Unbound_variable name -> Printf.sprintf "unbound variable: %s" name
