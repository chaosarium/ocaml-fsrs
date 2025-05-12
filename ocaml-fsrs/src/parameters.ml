type weights = float array

let _DEFAULT_WEIGHTS : weights = [| 
  0.4072; 1.1829; 3.1262; 15.4722; (* initial stability values for the 4 possible ratings *)
  7.2102; 
  0.5316; 1.0651; 0.0234; 1.6160; 0.1544; 
  1.0824; 1.9813; 0.0953; 0.2975; 2.2042; 
  0.2407; 2.9466; 0.5034; 0.6567; 
|]

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

let _DECAY = -. 0.5
let _FACTOR = 19.00 /. 81.00

let default_seed () = Timedesc.Timestamp.now () |> Timedesc.Timestamp.to_iso8601_milli

let default () = {
  request_retention = 0.9;
  maximum_interval = 36500;
  w = _DEFAULT_WEIGHTS;
  decay = _DECAY;
  factor = _FACTOR;
  enable_short_term = true;
  enable_fuzz = false;
  seed = default_seed ();
}


let forgetting_curve (params: t) ~(elapsed_days : float) ~(stability : float) : float =
  Float.pow (1.0 +. (_FACTOR *. (elapsed_days /. stability))) _DECAY

(* val init_difficulty : t -> rating:Models.rating -> float *)
(* pub fn init_difficulty(&self, rating: Rating) -> f64 {
    let rating_int: i32 = rating as i32;
    (self.w[4] - f64::exp(self.w[5] * (rating_int as f64 - 1.0)) + 1.0).clamp(1.0, 10.0)
} *)



(* val init_stability : t -> rating:Models.rating -> float *)
(* pub fn init_stability(&self, rating: Rating) -> f64 {
    let rating_int: i32 = rating as i32;
    self.w[(rating_int - 1) as usize].max(0.1)
} *)
let init_stability (params: t) ~(rating: Models.rating) : float =
  let rating_int: int = Models.rating_to_int rating in
  max 0.1 params.w.(rating_int - 1)




(* val init_difficulty : t -> rating:Models.rating -> float *)
(* val init_stability : t -> rating:Models.rating -> float *)
(* val next_interval : t -> stability:float -> elapsed_days:int -> float *)
(* val next_difficulty : t -> difficulty:float -> rating:Models.rating -> float *)
(* val short_term_stability : t -> stability:float -> rating:Models.rating -> float *)
(* val next_recall_stability :
  t ->
  difficulty:float ->
  stability:float ->
  retrievability:float ->
  rating:Models.rating ->
  float *)
(* val next_forget_stability :
  t ->
  difficulty:float ->
  stability:float ->
  retrievability:float ->
  float *)
(* val mean_reversion : t -> initial:float -> current:float -> float *)
(* val apply_fuzz : t -> interval:float -> elapsed_days:int -> float *)




























type fuzz_range = {
  start : float;
  end_ : float;
  factor : float;
}

let some_fuzz_ranges: fuzz_range list = [
  { start = 2.5; end_ = 7.0; factor = 0.15 };
  { start = 7.0; end_ = 20.0; factor = 0.1 };
  { start = 20.0; end_ = max_float; factor = 0.05 };
]