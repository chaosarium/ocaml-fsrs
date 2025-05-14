type weights = float array

let _DEFAULT_WEIGHTS : weights = [| 
  0.4072; 1.1829; 3.1262; 15.4722; (* initial stability values for the 4 possible ratings *)
  7.2102; 0.5316; (* initial difficulty base and offset factor *)
  1.0651; (* next difficulty curve *)
  0.0234; (* used in mean_reversion *)
  1.6160; 0.1544; 1.0824; (* recall stability params *)
  1.9813; 0.0953; 0.2975; 2.2042; (* forget stability params *)
  0.2407; 2.9466; (* stability modifiers for answering hard and easy *)
  0.5034; 0.6567; (* short_term_stability *)
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


(* val forgetting_curve : t -> elapsed_days:float -> stability:float -> float *)
let forgetting_curve (params: t) ~(elapsed_days : float) ~(stability : float) : float =
  (1.0 +. (_FACTOR *. (elapsed_days /. stability))) ** _DECAY

(* val init_stability : t -> rating:Models.rating -> float *)
let init_stability (params: t) ~(rating: Models.rating) : float =
  let rating_int: int = Models.rating_to_int rating in
  max 0.1 params.w.(rating_int - 1)

(* val init_difficulty : t -> rating:Models.rating -> float *)
let init_difficulty (params: t) ~(rating: Models.rating) : float =
  let rating_int: int = Models.rating_to_int rating in
  let unclamped_difficulty = params.w.(4) -. exp (params.w.(5) *. float_of_int (rating_int - 1)) +. 1.0 in
  min (max unclamped_difficulty 1.0) 10.0

(* TODO actually apply the fuzz if configured *)
(* val apply_fuzz : t -> interval:float -> elapsed_days:int -> float *)
let apply_fuzz (params: t) ~(interval: float) ~(elapsed_days: int) : float =
  (* let fuzz = Random.float 1.0 in *)
  interval

(* val next_interval : t -> stability:float -> elapsed_days:int -> float *)
let next_interval (params: t) ~(stability: float) ~(elapsed_days: int) : float =
  let new_interval = (
    (stability /. _FACTOR)
    *. 
    (params.request_retention ** (1.0 /. _DECAY) -. 1.0)
  )
    |> Float.round
    |> Utils.clamp_float 1.0 (float_of_int params.maximum_interval)
  in
  apply_fuzz params ~interval:new_interval ~elapsed_days

(* val mean_reversion : t -> initial:float -> current:float -> float *)
(* linearly interpolate by w[7] the initial and current values *)
let mean_reversion (params: t) ~(initial: float) ~(current: float) : float =
  (params.w.(7) *. initial) +. ((1.0 -. params.w.(7)) *. current) 

(* val next_difficulty : t -> difficulty:float -> rating:Models.rating -> float *)
let next_difficulty (params: t) ~(difficulty: float) ~(rating: Models.rating) : float =
  let rating_int: int = Models.rating_to_int rating in
  let next_difficulty = difficulty -. params.w.(6) *. (float_of_int (rating_int - 3)) in
  let mean_reversion = mean_reversion params ~initial:(init_difficulty params ~rating:Models.Easy) ~current:next_difficulty in
  Utils.clamp_float 1.0 10.0 mean_reversion

(* val short_term_stability : t -> stability:float -> rating:Models.rating -> float *)
(* computes stability after review in same day *)
let short_term_stability (params: t) ~(stability: float) ~(rating: Models.rating) : float =
  let rating_int: int = Models.rating_to_int rating in
  stability *. exp (params.w.(17) *. (float_of_int rating_int -. 3.0 +. params.w.(18)))

(* val next_recall_stability :
  t ->
  difficulty:float ->
  stability:float ->
  retrievability:float ->
  rating:Models.rating ->
  float *)
let next_recall_stability
    (params: t)
    ~(difficulty: float)
    ~(stability: float)
    ~(retrievability: float)
    ~(rating: Models.rating)
    : float =
  let modifier =
    match rating with
    | Models.Hard -> params.w.(15)
    | Models.Easy -> params.w.(16)
    | _ -> 1.0
  in
  let base =
    (Float.exp params.w.(8))
    *. (11.0 -. difficulty)
    *. (stability ** (-.params.w.(9)))
    *. (Float.expm1 ((1.0 -. retrievability) *. params.w.(10)))
  in
  stability *. ((base *. modifier) +. 1.0)

(* val next_forget_stability :
  t ->
  difficulty:float ->
  stability:float ->
  retrievability:float ->
  float *)
let next_forget_stability
    (params: t)
    ~(difficulty: float)
    ~(stability: float)
    ~(retrievability: float)
    : float =
  params.w.(11)
  *. (difficulty ** (-.params.w.(12)))
  *. (((stability +. 1.0) ** params.w.(13)) -. 1.0)
  *. Float.exp ((1.0 -. retrievability) *. params.w.(14))

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

(* val create_fuzz_range : start:float -> end_:float -> factor:float -> fuzz_range *)
let create_fuzz_range ~start ~end_ ~factor : fuzz_range =
  { start; end_; factor }

(* val get_fuzz_range : interval:float -> elapsed_days:int -> maximum_interval:int -> int * int *)
let get_fuzz_range ~(interval: float) ~(elapsed_days: int) ~(maximum_interval: int) : int * int =
  let delta = List.fold_left
    (fun acc fuzz_range ->
      acc +. fuzz_range.factor
      *. Float.max
        (Float.min interval fuzz_range.end_ -. fuzz_range.start)
        0.0
    )
    1.0
    some_fuzz_ranges
  in
  let i = Float.min interval (float_of_int maximum_interval) in
  let min_interval =
    let base = Float.max 2.0 (Float.round (i -. delta)) in
    let base =
      if i > float_of_int elapsed_days then
        Float.max base (float_of_int elapsed_days +. 1.0)
      else
        base
    in
    Float.min base (Float.min (Float.round (i +. delta)) (float_of_int maximum_interval))
  in
  let max_interval = Float.min (Float.round (i +. delta)) (float_of_int maximum_interval) in
  (int_of_float min_interval, int_of_float max_interval)