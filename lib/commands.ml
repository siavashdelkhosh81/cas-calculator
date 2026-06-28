(* Supported REPL commands, as (name, description) pairs. *)
let commands =
  [ ("/help", "show this list of commands")
  ; ("/clear", "clear the screen")
  ; ("/q", "quit the calculator")
  ]

let help_command () =
  let width =
    List.fold_left (fun acc (name, _) -> max acc (String.length name)) 0 commands
  in

  let render (name, desc) = Printf.sprintf "  %-*s  %s" width name desc in

  "commands:" :: List.map render commands


(* Clear screen + scrollback, move cursor to home. *)
let clear_command () =
  print_string "\027[2J\027[3J\027[H";
  Banner.print ();
  flush stdout
