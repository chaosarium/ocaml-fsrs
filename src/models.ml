type ts = Timedesc.Timestamp.t

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
let pp_card fmt (c : card) =
  Format.fprintf fmt
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
let show_card (c : card) =
  Format.asprintf "%a" pp_card c

type review_log = {
  rating: rating;
  elapsed_days: int;
  scheduled_days: int;
  state: state;
  reviewed_date: ts;
}
let pp_review_log fmt (r : review_log) =
  Format.fprintf fmt
    "{ rating = %s; elapsed_days = %d; scheduled_days = %d; state = %s; reviewed_date = %s }"
    (show_rating r.rating)
    r.elapsed_days
    r.scheduled_days
    (show_state r.state)
    (Timedesc.Timestamp.to_rfc3339 r.reviewed_date)
let show_review_log (r : review_log) =
  Format.asprintf "%a" pp_review_log r

type scheduling_info = {
  card: card;
  review_log: review_log;
} [@@deriving show]

module RatingMap = Map.Make(struct
  type t = rating
  let compare = compare
end)

type record_log = scheduling_info RatingMap.t 
let pp_record_log fmt (rl : record_log) =
  let open Format in
  fprintf fmt "@[<v 2>{";
  RatingMap.iter (fun rating sched_info ->
    fprintf fmt "@,%s: %a" (show_rating rating) pp_scheduling_info sched_info
  ) rl;
  fprintf fmt "@]@,}"

let show_record_log (rl : record_log) =
  Format.asprintf "%a" pp_record_log rl

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

let get_retrievability (card : card) ~(now : ts) (forgetting_curve : elapsed_days:float -> stability:float -> float) =
  match card.state with
  | New -> 0.0
  | _ ->
    let 
      elapsed_days = Utils.span_to_days (Timedesc.Timestamp.sub now card.last_review)
    in
    forgetting_curve ~elapsed_days:(float_of_int elapsed_days) ~stability:card.stability