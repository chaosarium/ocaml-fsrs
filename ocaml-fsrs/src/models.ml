type state = 
  | New 
  | Learning 
  | Review 
  | Relearning
[@@deriving show]

type rating = 
  | Again 
  | Hard 
  | Good 
  | Easy
[@@deriving show]

let rating_to_int (r: rating) = 
  match r with
  | Again -> 1
  | Hard -> 2
  | Good -> 3
  | Easy -> 4

let possible_ratings = [Again; Hard; Good; Easy]

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
let show_card (c : card) =
  Printf.sprintf
    "{ due = %s; stability = %.4f; difficulty = %.4f; elapsed_days = %d; scheduled_days = %d; reps = %d; lapses = %d; state = %s; last_review = %s }"
    (Timedesc.Timestamp.to_rfc3339 c.due)
    c.stability
    c.difficulty
    c.elapsed_days
    c.scheduled_days
    c.reps
    c.lapses
    (show_state c.state)
    (Timedesc.Timestamp.to_rfc3339 c.last_review)

type review_log = {
  rating: rating;
  elapsed_days: int;
  scheduled_days: int;
  state: state;
  reviewed_date: Timedesc.Timestamp.t;
}
let show_review_log (r : review_log) =
  Printf.sprintf
    "{ rating = %s; elapsed_days = %d; scheduled_days = %d; state = %s; reviewed_date = %s }"
    (show_rating r.rating)
    r.elapsed_days
    r.scheduled_days
    (show_state r.state)
    (Timedesc.Timestamp.to_rfc3339 r.reviewed_date)

type scheduling_info = {
  card: card;
  review_log: review_log;
}

module RatingMap = Map.Make(struct
  type t = rating
  let compare = compare
end)

type record_log = scheduling_info RatingMap.t

let new_card () =
  let now = Timedesc.Timestamp.now () in
  {
    due = now;
    stability = 0.0;
    difficulty = 0.0;
    elapsed_days = 0;
    scheduled_days = 0;
    reps = 0;
    lapses = 0;
    state = New;
    last_review = now;
  }

let get_retrievability (card : card) ~(now : Timedesc.Timestamp.t) (forgetting_curve : float -> float -> float) =
  match card.state with
  | New -> 0.0
  | _ ->
    let elapsed_days =
      0
      (* TODO *)
      (* let span = Ptime.diff now card.last_review in
      int_of_float (Ptime.Span.to_float_s span /. 86400.) *)
    in
    forgetting_curve (float_of_int elapsed_days) card.stability