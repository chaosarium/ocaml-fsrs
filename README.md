# fsrs-ocaml

OCaml implementation of the [FSRS](https://github.com/open-spaced-repetition/fsrs4anki) [algorithm](https://github.com/open-spaced-repetition/fsrs4anki/wiki/The-Algorithm).

For OCaml practice... and for... if someone on planet Earth ever wants an SRS library for their OCaml project[^1].

[^1]: Who knows if that someone would be ourself some day.

## Setup

```sh
opam install cmdliner
opam install ocamlformat
opam install base
```

## Quickstart

```ocaml
open Models

let () = 
  let params = Parameters.default () in
  let fsrs = Fsrs.create params in
  let card = Models.new_card () in
  
  let record_log = Fsrs.repeat fsrs card (Timedesc.Timestamp.now ()) in
  Models.RatingMap.iter (fun rating sched_info ->
    Printf.printf "If you answer \"%s\":\n" (Models.show_rating rating);
    Printf.printf "  Next card: %s\n" (Models.show_card sched_info.card);
    Printf.printf "  Review log: %s\n" (Models.show_review_log sched_info.review_log);
    print_endline ""
  ) record_log
```

Refer to `demo.ml` for the same example, or run the example by running `dune exec demo`.

## Development

```sh
# to build
dune build
# to run unit tests
dune runtest
# to run the quickstart example
dune build && dune exec demo
```

Or refer to commands in `justfile`.

## Release stuff

Something like

```sh
git tag 0.1.0-alpha2
git push --tags
opam publish
```

## Acknowledgements

This implementation is largely based on the Rust implementation [rs-fsrs](https://github.com/open-spaced-repetition/rs-fsrs) (MIT license).
