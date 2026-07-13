open Base
open Ast
open Lexer

let parser (all_tokens : token list) : expr =
  let remaining_tokens = ref all_tokens in

  let peek () =
    match !remaining_tokens with
    | [] -> None
    | next_token :: _ -> Some next_token
  in

  let consume () =
    match !remaining_tokens with
    | [] -> ()
    | _used_token :: rest -> remaining_tokens := rest
  in

  let rec parse_factor () =
    match peek () with
    | Some (NUM number_value) ->
        consume ();
        Num number_value
    | Some (VAR variable_name) ->
        consume ();
        Var variable_name
    | Some LPAREN ->
        consume ();
        let inner_expression = parse_expression () in
        (match peek () with
         | Some RPAREN -> consume ()
         | _ -> raise (Calc_error.Calc_error Missing_rparen));
        inner_expression
    | Some SIN -> consume (); Func ("sin", parse_factor ())
    | Some COS -> consume (); Func ("cos", parse_factor ())
    | Some TAN -> consume (); Func ("tan", parse_factor ())
    | Some LOG -> consume (); Func ("log", parse_factor ())
    | Some LN -> consume (); Func ("ln", parse_factor ())
    | Some LOG2 -> consume (); Func ("log2", parse_factor ())
    | Some EXP -> consume (); Func ("exp", parse_factor ())
    | Some ABS -> consume (); Func ("abs", parse_factor ())
    | Some SQRT -> consume (); Func ("sqrt", parse_factor ())
    | Some FLOOR -> consume (); Func ("floor", parse_factor ())
    | Some CEIL -> consume (); Func ("ceil", parse_factor ())
    | Some ROUND -> consume (); Func ("round", parse_factor ())
    | Some ASIN -> consume (); Func ("asin", parse_factor ())
    | Some ACOS -> consume (); Func ("acos", parse_factor ())
    | Some ATAN -> consume (); Func ("atan", parse_factor ())
    | Some SINH -> consume (); Func ("sinh", parse_factor ())
    | Some COSH -> consume (); Func ("cosh", parse_factor ())
    | Some TANH -> consume (); Func ("tanh", parse_factor ())
    | Some DIFF -> consume (); parse_diff ()
    | Some EXPAND -> consume (); Func ("expand", parse_factor ())
    | Some FACTOR -> consume (); Func ("factor", parse_factor ())
    | Some MINUS -> consume (); Neg (parse_power ())
    | None -> raise (Calc_error.Calc_error Unexpected_end)
    | Some other -> raise (Calc_error.Calc_error (Unexpected_token (string_of_token other)))


  (* diff ( <expression> , <variable> ) — the DIFF token is already consumed.
     Unlike the one-argument functions, diff always requires parentheses. *)
  and parse_diff () =
    (match peek () with
     | Some LPAREN -> consume ()
     | Some other -> raise (Calc_error.Calc_error (Unexpected_token (string_of_token other)))
     | None -> raise (Calc_error.Calc_error Unexpected_end));
    let body = parse_expression () in
    (match peek () with
     | Some COMMA -> consume ()
     | _ -> raise (Calc_error.Calc_error Expected_comma));
    let variable =
      match peek () with
      | Some (VAR name) -> consume (); name
      | _ -> raise (Calc_error.Calc_error Expected_variable_name)
    in
    (match peek () with
     | Some RPAREN -> consume ()
     | _ -> raise (Calc_error.Calc_error Missing_rparen));
    Diff (body, variable)

  and parse_power () =
    let base = parse_factor () in

    match peek () with
    | Some CARET -> consume (); Expo (base, parse_power ())
    | _ -> base

  and parse_term () =
    let left_side = ref (parse_power ()) in
    let continue = ref true in

    while !continue do
      match peek () with
      | Some STAR -> consume (); left_side := Mul (!left_side, parse_power ())
      | Some SLASH -> consume (); left_side := Div (!left_side, parse_power ())
      | _ -> continue := false
    done;

    !left_side

  and parse_expression () =
    let left_side = ref (parse_term ()) in
    let continue = ref true in

    while !continue do
      match peek () with
      | Some PLUS  -> consume (); left_side := Add (!left_side, parse_term ())
      | Some MINUS -> consume (); left_side := Sub (!left_side, parse_term ())
      | _ -> continue := false
    done;

    !left_side
  in

  let parsed_tree = parse_expression () in

  (match peek () with
   | None -> ()
   | Some _ -> raise (Calc_error.Calc_error Trailing_input));
  parsed_tree


let parse (all_tokens: token list) : statement =
  match all_tokens with
  | LET :: VAR name :: EQUALS :: rest ->  Let_binding (name, parser rest)
  | all -> Expression (parser all)
