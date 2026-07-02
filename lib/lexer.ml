type token =
  | NUM of float
  | PLUS
  | MINUS
  | STAR
  | SLASH
  | CARET
  | LPAREN
  | RPAREN
  | VAR of string

let string_of_token = function
  | NUM n -> Printf.sprintf "NUM %g" n
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | STAR -> "STAR"
  | CARET -> "CARET"
  | SLASH -> "SLASH"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | VAR name -> Printf.sprintf "VAR %s" name

let is_digit character = character >= '0' && character <= '9'

let is_alpha character =
  (character >= 'a' && character <= 'z')
  || (character >= 'A' && character <= 'Z')

(* Turn a string into a list of tokens. *)
let tokenize (input : string) : token list =
  let length = String.length input in

  (* Walk through the string one position at a time, collecting tokens. *)
  let rec scan position tokens_so_far =
    if position >= length then List.rev tokens_so_far

    else
      let character = input.[position] in
      match character with
      | ' ' -> scan (position + 1) tokens_so_far (* skip whitespace *)
      | '+' -> scan (position + 1) (PLUS :: tokens_so_far)
      | '*' -> scan (position + 1) (STAR :: tokens_so_far)
      | '^' -> scan (position + 1) (CARET :: tokens_so_far)
      | '/' -> scan (position + 1) (SLASH :: tokens_so_far)
      | '-' -> scan (position + 1) (MINUS :: tokens_so_far)
      | ')' -> scan (position + 1) (RPAREN :: tokens_so_far)
      | '(' -> scan (position + 1) (LPAREN :: tokens_so_far)
      | c when is_digit c || c = '.' -> read_number position tokens_so_far
      | c when is_alpha c -> read_identifier position tokens_so_far
      | _ -> raise (Error.Calc_error (Invalid_char character))

  (* Grab a run of digits (and one dot) as a single number token. *)
  and read_number start_position tokens_so_far =
    let end_position = ref start_position in
    while
      !end_position < length
      && (is_digit input.[!end_position] || input.[!end_position] = '.')
    do
      incr end_position
    done;

    let number_text =
      String.sub input start_position (!end_position - start_position)
    in

    (match float_of_string_opt number_text with
     | Some value -> scan !end_position (NUM value :: tokens_so_far)
     | None -> raise (Error.Calc_error (Invalid_number number_text)))

  (* Grab a run of letters as a single variable token. *)
  and read_identifier start_position tokens_so_far =
    let end_position = ref start_position in

    while !end_position < length && is_alpha input.[!end_position] do
      incr end_position
    done;

    let identifier_text =
      String.sub input start_position (!end_position - start_position)
    in

    scan !end_position (VAR identifier_text :: tokens_so_far)
  in

  (* Start the function *)
  scan 0 []
