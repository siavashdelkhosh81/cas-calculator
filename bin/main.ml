open Base
open Stdio

let listener (input : string) =
  match input with
  | "/help" -> List.iter (Calculator.Commands.help_command ()) ~f:print_endline
  | "/clear" -> Calculator.Commands.clear_command ()
  | "/install_skill" -> (
      match Calculator.Commands.install_skill () with
      | Ok message -> print_endline message
      | Error code -> print_endline ("error: " ^ Calculator.Calc_error.to_string code))
  | expr -> (
      match Calculator.Eval.evaluate expr with
      | Ok result -> print_endline ("= " ^ result)
      | Error code -> print_endline ("error: " ^ Calculator.Calc_error.to_string code))

(* Interactive REPL: banner, then read-eval-print until /q or EOF. *)
let repl () =
  Calculator.Banner.print ();
  let rec loop () =
    Calculator.Banner.prompt ();
    match In_channel.input_line In_channel.stdin with
    | None | Some "/q" -> printf "\nbye\n"
    | Some input -> listener input; loop ()
  in
  loop ()


let calculate (expression : string) =
  match Calculator.Eval.evaluate expression with
  | Ok result -> print_endline result
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
