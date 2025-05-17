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

## Running stuff

```sh
# build
dune build
# run unit tests
dune runtest
```

## Acknowledgements

This implementation is largely based on the Rust implementation [rs-fsrs](https://github.com/open-spaced-repetition/rs-fsrs) (MIT license).
