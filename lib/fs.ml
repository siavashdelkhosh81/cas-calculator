(* Create [path] and any missing parent directories, like mkdir -p. *)
let rec make_dirs path =
  if not (Stdlib.Sys.file_exists path) then (
    make_dirs (Stdlib.Filename.dirname path);
    Stdlib.Sys.mkdir path 0o755)
