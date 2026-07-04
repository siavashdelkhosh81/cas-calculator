open Base

(* Supported REPL commands, as (name, description) pairs. *)
let commands =
  [ ("/help", "show this list of commands")
  ; ("/clear", "clear the screen")
  ; ("/q", "quit the calculator")
  ]

let help_command () =
  let width =
    List.fold commands ~init:0 ~f:(fun acc (name, _) -> max acc (String.length name))
  in

  let render (name, desc) = Printf.sprintf "  %-*s  %s" width name desc in

  "commands:" :: List.map commands ~f:render


let install_skill () =
  if (true) then Ok "Skills installed"
  else
    Error Calc_error.Failed_to_install

(* Clear screen + scrollback, move cursor to home. *)
let clear_command () =
  Stdio.print_string "\027[2J\027[3J\027[H";
  Banner.print ();
  Stdio.Out_channel.flush Stdio.stdout
