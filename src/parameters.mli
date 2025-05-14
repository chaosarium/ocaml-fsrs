val _DEFAULT_WEIGHTS : float array (* must be of length 19. how to enforce at type level? *)

type t = {
  request_retention : float;
  maximum_interval : int;
  w : float array;
  decay : float;
  factor : float;
  enable_short_term : bool;
  enable_fuzz : bool;
  seed : string;
}

val _DECAY : float
val _FACTOR : float

val default : unit -> t

val forgetting_curve : t -> elapsed_days:float -> stability:float -> float
val init_stability : t -> rating:Models.rating -> float
val init_difficulty : t -> rating:Models.rating -> float
val next_interval : t -> stability:float -> elapsed_days:int -> float
val next_difficulty : t -> difficulty:float -> rating:Models.rating -> float
val short_term_stability : t -> stability:float -> rating:Models.rating -> float
val next_recall_stability :
  t ->
  difficulty:float ->
  stability:float ->
  retrievability:float ->
  rating:Models.rating ->
  float
val next_forget_stability :
  t ->
  difficulty:float ->
  stability:float ->
  retrievability:float ->
  float
val mean_reversion : t -> initial:float -> current:float -> float
val apply_fuzz : t -> interval:float -> elapsed_days:int -> float



type fuzz_range = {
  start : float;
  end_ : float;
  factor : float;
}
val create_fuzz_range : start:float -> end_:float -> factor:float -> fuzz_range
val get_fuzz_range : interval:float -> elapsed_days:int -> maximum_interval:int -> int * int
val some_fuzz_ranges: fuzz_range list

(* implement random number generator and seed if that's what we want. *)