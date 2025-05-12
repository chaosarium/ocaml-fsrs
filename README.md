# fsrs-ocaml

OCaml implementation of the [FSRS](https://github.com/open-spaced-repetition/fsrs4anki) [algorithm](https://github.com/open-spaced-repetition/fsrs4anki/wiki/The-Algorithm).

For OCaml practice... and for... if someone on planet Earth ever wants an SRS library for their OCaml project[^1].

[^1]: Who knows if that someone would be ourself some day.

## Setup

```sh
opam install cmdliner
opam install ocamlformat
opam install base
opam install ptime
```

## Testing

```sh
dune build && dune exec fsrs_ocaml -- -n Alice -c 3
```

