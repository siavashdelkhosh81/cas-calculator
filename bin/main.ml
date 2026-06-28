(* Handler and listenr *)
let () =
  Calculator.Banner.print ();
  try
    while true do
      Calculator.Banner.prompt ();
      let line = input_line stdin in
      match line with
      | "/q" -> raise End_of_file
      | "/help" -> List.iter print_endline (Calculator.Commands.help_command ())
      | "/clear" -> Calculator.Commands.clear_command ()
      | expr -> print_endline ("= " ^ expr)
    done
  with End_of_file -> Printf.printf "\nbye\n"
