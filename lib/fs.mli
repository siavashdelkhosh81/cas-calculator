(** Filesystem helpers. *)

(** [make_dirs path] creates [path] and any missing parent directories,
    like [mkdir -p]. Does nothing if [path] already exists. *)
val make_dirs : string -> unit
