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
         | _ -> failwith "expected a closing )");
        inner_expression
    | _ -> failwith "expected a number, variable, or ("

  and parse_term () =
    let left_side = ref (parse_factor ()) in
    while peek () = Some STAR do
      consume ();
      let right_side = parse_factor () in
      left_side := Mul (!left_side, right_side)
    done;
    !left_side

  and parse_expression () =
    let left_side = ref (parse_term ()) in
    while peek () = Some PLUS do
      consume ();
      let right_side = parse_term () in
      left_side := Add (!left_side, right_side)
    done;
    !left_side
  in

  let parsed_tree = parse_expression () in

  (match peek () with
   | None -> ()
   | Some _ -> failwith "unexpected tokens after the expression");
  parsed_tree
