open Base

type token =
  | NUM of float
  | PLUS
  | MINUS
  | STAR
  | SLASH
  | CARET
  | SQRT
  | SIN
  | LET
  | EQUALS
  | COS
  | LOG
  | LN
  | LOG2
  | EXP
  | ABS
  | FLOOR
  | CEIL
  | ROUND
  | ASIN
  | ACOS
  | ATAN
  | SINH
  | COSH
  | TANH
  | TAN
  | LPAREN
  | RPAREN
  | VAR of string

let string_of_token = function
  | NUM n -> Printf.sprintf "NUM %g" n
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | STAR -> "STAR"
  | CARET -> "CARET"
  | SQRT -> "SQRT"
  | SIN -> "SIN"
  | LOG -> "LOG"
  | LN -> "LN"
  | LOG2 -> "LOG2"
  | EXP -> "EXP"
  | FLOOR -> "FLOOR"
  | CEIL -> "CEIL"
  | ROUND -> "ROUND"
  | ABS -> "ABS"
  | ASIN -> "ASIN"
  | ACOS -> "ACOS"
  | ATAN -> "ATAN"
  | LET -> "LET"
  | EQUALS -> "EQUALS"
  | SINH -> "SINH"
  | COSH -> "COSH"
  | TANH -> "TANH"
  | COS -> "COS"
  | TAN -> "TAN"
  | SLASH -> "SLASH"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | VAR name -> Printf.sprintf "VAR %s" name

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
      | '=' -> scan (position + 1) (EQUALS :: tokens_so_far)
      | '^' -> scan (position + 1) (CARET :: tokens_so_far)
      | '/' -> scan (position + 1) (SLASH :: tokens_so_far)
      | '-' -> scan (position + 1) (MINUS :: tokens_so_far)
      | ')' -> scan (position + 1) (RPAREN :: tokens_so_far)
      | '(' -> scan (position + 1) (LPAREN :: tokens_so_far)
      | c when Char.is_digit c || Char.equal c '.' -> read_number ~start_position:position ~tokens_so_far:tokens_so_far
      | c when Char.is_alpha c -> read_identifier ~start_position:position ~tokens_so_far:tokens_so_far
      | _ -> raise (Calc_error.Calc_error (Invalid_char character))

  (* Grab a run of digits (and one dot) as a single number token. *)
  and read_number ~(start_position:int) ~(tokens_so_far: token list) =
    let end_position = ref start_position in
    while
      !end_position < length
      && (Char.is_digit input.[!end_position] || Char.equal input.[!end_position] '.')
    do
      Int.incr end_position
    done;

    let number_text =
      String.sub input ~pos:start_position ~len:(!end_position - start_position)
    in

    (match Float.of_string_opt number_text with
     | Some value -> scan !end_position (NUM value :: tokens_so_far)
     | None -> raise (Calc_error.Calc_error (Invalid_number number_text)))

  (* Grab a run of letters (digits allowed after the first letter, so names
     like log2 and log10 work) as a single identifier token. *)
  and read_identifier ~(start_position: int) ~(tokens_so_far: token list) =
    let end_position = ref start_position in

    while !end_position < length && Char.is_alphanum input.[!end_position] do
      Int.incr end_position
    done;

    let identifier_text =
      String.sub input ~pos:start_position ~len:(!end_position - start_position)
    in

    (* A known function name becomes its own token; anything else is a variable. *)
    let token =
      match identifier_text with
      | "sin" -> SIN
      | "cos" -> COS
      | "tan" -> TAN
      | "log" -> LOG
      | "log10" -> LOG (* alias: log is already base 10 *)
      | "ln" -> LN
      | "log2" -> LOG2
      | "exp" -> EXP
      | "abs" -> ABS
      | "floor" -> FLOOR
      | "ceil" -> CEIL
      | "round" -> ROUND
      | "asin" -> ASIN
      | "acos" -> ACOS
      | "atan" -> ATAN
      | "sinh" -> SINH
      | "cosh" -> COSH
      | "tanh" -> TANH
      | "sqrt" -> SQRT
      | "let" -> LET
      | _ -> VAR identifier_text
    in

    scan !end_position (token :: tokens_so_far)
  in

  (* Start the function *)
  scan 0 []
