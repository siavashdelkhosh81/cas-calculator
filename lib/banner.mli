(** Startup banner for the calculator REPL. *)

(** [print ()] writes the full startup banner to [stdout] and flushes. *)
val print : unit -> unit

(** [animate ()] plays a short gradient-shimmer animation of the banner,
    leaving the resting banner on screen. Falls back to [print] when
    [stdout] is not a tty. *)
val animate : unit -> unit

(** [prompt ()] writes the input prompt to [stdout] and flushes. *)
val prompt : unit -> unit
