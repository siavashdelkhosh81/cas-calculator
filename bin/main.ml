let listener (input:string) =
  match input with
  | "/q" -> raise End_of_file
  | "/help" -> List.iter print_endline (Calculator.Commands.help_command ())
  | "/clear" -> Calculator.Commands.clear_command ()
  | expr ->
      print_endline
        ("= " ^ Calculator.Eval.eval (Calculator.Parser.parser (Calculator.Lexer.tokenize expr)))

(* Handler and listenr *)
let () =
  Calculator.Banner.print ();
  try
    while true do
      Calculator.Banner.prompt ();
      let input = input_line stdin in
      listener input
    done
  with End_of_file -> Printf.printf "\nbye\n"
