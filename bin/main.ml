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

(* Handler and listener *)
let () =
  Calculator.Banner.print ();
  let rec loop () =
    Calculator.Banner.prompt ();
    match In_channel.input_line In_channel.stdin with
    | None | Some "/q" -> printf "\nbye\n"
    | Some input -> listener input; loop ()
  in
  loop ()
