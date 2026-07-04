(** Calculator REPL commands. *)

(** [commands] lists every supported command as [(name, description)] pairs. *)
val commands : (string * string) list

(** [help_command ()] returns the help text as a list of lines: a heading
    followed by one aligned line per entry in {!commands}. *)
val help_command : unit -> string list


val clear_command : unit -> unit

(** [install_skill ()] writes the calculator skill file into the skills
    directory of every supported AI tool found in the user's home
    ([.claude], [.cursor], [.codex]). Returns [Ok message] naming the tools
    it installed for, or [Error Failed_to_install] if home can't be found,
    no tool directory exists, or a write fails. *)
val install_skill : unit -> (string, Calc_error.error) result
