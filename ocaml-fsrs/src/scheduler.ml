open Models
open Utils

type t = {
  parameters : Parameters.t;
  last : card;
  current : card;
  now : Timedesc.Timestamp.t;
  next : record_log;
}

let init_seed scheduler =
  let time = Int64.to_int (Int64.div (Timedesc.Timestamp.to_float_s scheduler.now |> (fun s -> Int64.of_float (s *. 1000.))) 1L) in
  let reps = scheduler.current.reps in
  let mul = scheduler.current.difficulty *. scheduler.current.stability in
  let seed_str = Printf.sprintf "%d_%d_%f" time reps mul in
  { scheduler with parameters = { scheduler.parameters with seed = seed_str } }

(* TODO maybe init_seed should mutate the scheduler *)
let create (parameters : Parameters.t) (card : card) (now : Timedesc.Timestamp.t) =
  let elapsed_days =
    match card.state with
    | New -> 0
    | _ -> span_to_days (Timedesc.Timestamp.sub now card.last_review)
  in
  let current = { card with
    elapsed_days;
    last_review = now;
    reps = card.reps + 1;
  } in
  init_seed {
    parameters;
    last = card;
    current = current;
    now;
    next = RatingMap.empty;
  }

let build_log scheduler rating =
  {
    rating;
    state = scheduler.current.state;
    elapsed_days = scheduler.current.elapsed_days;
    scheduled_days = scheduler.current.scheduled_days;
    reviewed_date = scheduler.now;
  }

module type Scheduler = sig
  val preview : t -> card -> record_log
  val review : t -> card -> rating -> (card * scheduling_info)
end