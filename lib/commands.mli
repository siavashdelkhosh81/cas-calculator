(** Calculator REPL commands. *)

(** [commands] lists every supported command as [(name, description)] pairs. *)
val commands : (string * string) list

(** [help_command ()] returns the help text as a list of lines: a heading
    followed by one aligned line per entry in {!commands}. *)
val help_command : unit -> string list


val clear_command : unit -> unit
