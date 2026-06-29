let listener (input:string) =
  match input with
  | "/q" -> raise End_of_file
  | "/help" -> List.iter print_endline (Calculator.Commands.help_command ())
  | "/clear" -> Calculator.Commands.clear_command ()
  | expr -> (
      match Calculator.Eval.evaluate expr with
      | Ok result -> print_endline ("= " ^ result)
      | Error code -> print_endline ("error: " ^ Calculator.Error.to_string code))

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
