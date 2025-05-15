(* seed *)

type seed =
  | String of string
  | Empty
  | Default

let default_seed () =
  let ms =
    let now = Timedesc.Timestamp.now () in
    let seconds = Timedesc.Timestamp.to_float_s now in
    int_of_float (seconds *. 1000.)
  in
  String (string_of_int ms)

let new_seed v =
  match v with
  | "" -> default_seed ()
  | s  -> String s

let rec seed_inner_str s =
  match s with
  | String str -> str
  | Empty -> seed_inner_str (default_seed ())
  | Default -> seed_inner_str (default_seed ())

(* alea *)

type alea_t = {
  mutable s0 : float;
  mutable s1 : float;
  mutable s2 : float;
  mutable c  : float;
}

let _TWO_TO_THE_POWER_OF_32 = 4294967296.
let _TWO_TO_THE_POWER_OF_21 = 2097152.
let _TWO_TO_THE_POWER_OF_53 = 9007199254740992.

(* a string-to-float hash function *)
let create_mash () =
  let n = ref 0xefc8249d in
  fun (seed : seed) ->
    let n_local = ref (float_of_int !n) in
    let seed_str = seed_inner_str seed in
    for i = 0 to String.length seed_str - 1 do
      n_local := !n_local +. float_of_int (Char.code seed_str.[i]);
      let mut_h = 0.02519603282416938 *. !n_local in
      let mut_n = float_of_int (int_of_float mut_h) in
      let mut_h = mut_h -. mut_n in
      let mut_h = mut_h *. mut_n in
      let mut_n = float_of_int (int_of_float mut_h) in
      let mut_h = mut_h -. mut_n in
      n_local := mut_n +. mut_h *. _TWO_TO_THE_POWER_OF_32;
    done;
    n := int_of_float !n_local;
    (!n_local *. (1. /. _TWO_TO_THE_POWER_OF_32))

let create_alea seed =
  let m = create_mash () in
  let blank_seed = new_seed " " in
  let c = 1.0 in 
  let s0 = m blank_seed in
  let s1 = m blank_seed in
  let s2 = m blank_seed in
  
  let s0 = s0 -. (m seed) in
  let s0 = if s0 < 0. then s0 +. 1. else s0 in
  let s1 = s1 -. (m seed) in
  let s1 = if s1 < 0. then s1 +. 1. else s1 in
  let s2 = s2 -. (m seed) in
  let s2 = if s2 < 0. then s2 +. 1. else s2 in
  { c; s0; s1; s2 }

let alea_next (alea: alea_t) : alea_t * float = 
  let t = (2091639.0 *. alea.s0) +. (alea.c /. _TWO_TO_THE_POWER_OF_32) in
  let s0 = alea.s1 in
  let s1 = alea.s2 in
  let c = floor t in
  let s2 = t -. c in
  ({ s0; s1; s2; c }, s2)

(* prng *)

type prng_t = {
  mutable xg : alea_t;
}

let wrap_to_i32 (input : float) : int =
  let n = int_of_float (floor input) in
  if n >= 0x80000000 then n - 0x100000000 else n

let prng_new seed =
  { xg = create_alea seed }

let prng_gen_next prng =
  let (xg', v) = alea_next prng.xg in
  prng.xg <- xg';
  v

let prng_int32 prng =
  wrap_to_i32 (prng_gen_next prng *. _TWO_TO_THE_POWER_OF_32)

let prng_double prng =
  let v1 = prng_gen_next prng in
  let v2 = prng_gen_next prng in
  ((float_of_int (int_of_float (v1 *. _TWO_TO_THE_POWER_OF_21))) *. _TWO_TO_THE_POWER_OF_53) +. v2

let prng_get_state prng =
  prng.xg

let prng_import_state prng state =
  { xg = state }