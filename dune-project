(lang dune 3.17)

(name ocaml-fsrs)

(generate_opam_files true)

(version 0.1.0~alpha2)

(source
 (github chaosarium/ocaml-fsrs))

(authors "Lómenoirë Mortecc. <chaosarium@duck.com>")

(maintainers "Lómenoirë Mortecc. <chaosarium@duck.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name ocaml-fsrs)
 (synopsis "OCaml implementation of the FSRS scheduler")
 (description "A library implementing the FSRS (Free Spaced Repetition Scheduler) algorithm in OCaml.")
 (depends
   ocaml
   base
   timedesc
 )
 (tags ("spaced-repetition" "srs" "fsrs" "anki" "learning" "ocaml" "algorithm")))
