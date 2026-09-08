(lang dune 3.23)

(name calculator)

(generate_opam_files true)

(source
 (github siavashdelkhosh81/cas-calculator))

(authors "Siavash delkhosh <47573636+siavashdelkhosh81@users.noreply.github.com>")

(maintainers "Siavash delkhosh <47573636+siavashdelkhosh81@users.noreply.github.com>")

(license LICENSE)

(documentation https://github.com/siavashdelkhosh81/cas-calculator#readme)

(package
 (name calculator)
 (synopsis "A fast, open-source computer algebra system built in OCaml")
 (description "Lex, parse, and evaluate: an algebra engine written in OCaml, with exact rational arithmetic, symbolic differentiation, simplification, and equation solving.")
 (depends ocaml base stdio zarith)
 (tags
  (ocaml cas computer-algebra-system calculator symbolic-math)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
