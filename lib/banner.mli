(** Startup banner for the calculator REPL. *)

(** [print ()] writes the full startup banner to [stdout] and flushes. *)
val print : unit -> unit

(** [prompt ()] writes the input prompt to [stdout] and flushes. *)
val prompt : unit -> unit
