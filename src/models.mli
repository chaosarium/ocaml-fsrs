type ts = Timedesc.Timestamp.t
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
val possible_ratings : rating list
type card = {
  due: ts;
  stability: float;
  difficulty: float;
  elapsed_days: int;
  scheduled_days: int;
  reps: int;
  lapses: int;
  state: state;
  last_review: ts;
}
type review_log = {
  rating: rating;
  elapsed_days: int;
  scheduled_days: int;
  state: state;
  reviewed_date: ts;
}
type scheduling_info = {
  card: card;
  review_log: review_log;
}

module RatingMap : Map.S with type key = rating
type record_log = scheduling_info RatingMap.t

val new_card: unit -> card
val get_retrievability: card -> now:ts -> (elapsed_days:float -> stability:float -> float) -> float

val show_state : state -> string
val show_rating : rating -> string
val show_card : card -> string
val show_review_log : review_log -> string
(* val show_scheduling_info : scheduling_info -> string *)

val rating_to_int : rating -> int