type state = 
  | New 
  | Learning 
  | Review 
  | Relearning
type rating = 
  | Again 
  | Hard 
  | Good 
  | Easy
type card = {
  due: Timedesc.Timestamp.t;
  stability: float;
  difficulty: float;
  elapsed_days: int;
  scheduled_days: int;
  reps: int;
  lapses: int;
  state: state;
  last_review: Timedesc.Timestamp.t;
}
type review_log = {
  rating: rating;
  elapsed_days: int;
  scheduled_days: int;
  state: state;
  reviewed_date: Timedesc.Timestamp.t;
}
type scheduling_info = {
  card: card;
  review_log: review_log;
}

module RatingMap : Map.S with type key = rating
type record_log = scheduling_info RatingMap.t

val new_card: unit -> card
val get_retrievability: card -> now:Timedesc.Timestamp.t -> (float -> float -> float) -> float
(* TODO specify on type level the float -> float -> float is a forgetting curve *)

val show_state : state -> string
val show_rating : rating -> string
val show_card : card -> string
val show_review_log : review_log -> string
(* val show_scheduling_info : scheduling_info -> string *)

val rating_to_int : rating -> int