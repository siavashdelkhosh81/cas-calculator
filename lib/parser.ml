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
         | _ -> raise (Error.Calc_error Missing_rparen));
        inner_expression
    | Some SIN -> consume (); Func ("sin", parse_factor ())
    | Some COS -> consume (); Func ("cos", parse_factor ())
    | None -> raise (Error.Calc_error Unexpected_end)
    | Some other -> raise (Error.Calc_error (Unexpected_token (string_of_token other)))


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
   | Some _ -> raise (Error.Calc_error Trailing_input));
  parsed_tree
