open Base
open Stdio

(* Turn one line of user input into the text to display plus the environment
   to carry into the next iteration. Returns "" when there is nothing to
   print (e.g. /clear, which redraws the screen itself). *)
let listener ~env (input : string) : string * float Map.M(String).t =
  match input with
  | "/help" -> (String.concat ~sep:"\n" (Calculator.Commands.help_command ()), env)
  | "/clear" -> Calculator.Commands.clear_command (); ("", env)
  | "/install_skill" -> (
      match Calculator.Commands.install_skill () with
      | Ok message -> (message, env)
      | Error code -> ("error: " ^ Calculator.Calc_error.to_string code, env))
  | command_input -> (
      match Calculator.Eval.evaluate ~env ~input:command_input with
      | Ok (result, new_env) -> ("= " ^ result, new_env)
      | Error code -> ("error: " ^ Calculator.Calc_error.to_string code, env))

(* Interactive REPL: banner, then read-eval-print until /q or EOF. *)
let repl () =
  Calculator.Banner.print ();
  let rec loop env =
    Calculator.Banner.prompt ();
    match In_channel.input_line In_channel.stdin with
    | None | Some "/q" -> printf "\nbye\n"
    | Some input ->
        let output, env = listener ~env input in
        (match output with
         | "" -> ()
         | output -> print_endline output);
        loop env
  in
  loop (Map.empty (module String))


let calculate (expression : string) =
  let env = Map.empty(module String) in
  match Calculator.Eval.evaluate ~env:env ~input:expression with
  | Ok (result, env) -> print_endline result
  | Error code ->
      eprintf "error: %s\n" (Calculator.Calc_error.to_string code);
      Stdlib.exit 1

let () =
  match Sys.get_argv () with
  | [| _ |] -> repl ()
  | [| _; "calculate"; expression |] -> calculate expression
  | _ ->
      eprintf "usage:\n  calculator                        start the interactive REPL\n  calculator calculate \"<expr>\"     evaluate once and print the result\n";
      Stdlib.exit 2
