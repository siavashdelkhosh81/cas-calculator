open Base

(* Supported REPL commands, as (name, description) pairs. *)
let commands =
  [ ("/help", "Show this list of commands")
  ; ("/clear", "Clear the screen")
  ; ("/q", "Quit the calculator")
  ; ("/install_skill", "Install AI Skills so your AI tools can use this for calculation")
  ]

let help_command () =
  let width =
    List.fold commands ~init:0 ~f:(fun acc (name, _) -> max acc (String.length name))
  in

  let render (name, desc) = Printf.sprintf "  %-*s  %s" width name desc in

  "commands:" :: List.map commands ~f:render


let install_skill () =
  let supported_tools = [ ".claude"; ".cursor"; ".codex" ] in

  match Sys.getenv "HOME" with
  | None -> Error Calc_error.Failed_to_install
  | Some home ->
      let ( / ) = Stdlib.Filename.concat in

      (* Keep only the tools the user actually has. *)
      let found_tools =
        List.filter supported_tools ~f:(fun tool ->
            Stdlib.Sys.file_exists (home / tool))
      in

      (* Run through the list and install the skill. *)
      (match found_tools with
       | [] -> Error Calc_error.No_ai_tool_found
       | tools -> (
           try
             List.iter tools ~f:(fun tool ->
                 let skill_dir = home / tool / "skills" / "calculator" in
                 Fs.make_dirs skill_dir;
                 Stdio.Out_channel.write_all (skill_dir / "SKILL.md")
                   ~data:Skill.skill_text);
             Ok ("Skills installed for: " ^ String.concat ~sep:", " tools)
           with _ -> Error Calc_error.Failed_to_install))

(* Clear screen + scrollback, move cursor to home. *)
let clear_command () =
  Stdio.print_string "\027[2J\027[3J\027[H";
  Banner.print ();
  Stdio.Out_channel.flush Stdio.stdout
