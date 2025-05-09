# fsrs-ocaml

OCaml implementation of the [FSRS](https://github.com/open-spaced-repetition/fsrs4anki) [algorithm](https://github.com/open-spaced-repetition/fsrs4anki/wiki/The-Algorithm).


## Setup

```sh
opam install cmdliner
```

## Testing

```sh
dune build && dune exec fsrs_ocaml -- -n Alice -c 3
```