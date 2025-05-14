open Models
open Utils

type t = {
  parameters : Parameters.t;
  last : card;
  current : card;
  now : ts;
  next : record_log;
}

let init_seed sched =
  let time = Int64.to_int (Int64.div (Timedesc.Timestamp.to_float_s sched.now |> (fun s -> Int64.of_float (s *. 1000.))) 1L) in
  let reps = sched.current.reps in
  let mul = sched.current.difficulty *. sched.current.stability in
  let seed_str = Printf.sprintf "%d_%d_%f" time reps mul in
  { sched with parameters = { sched.parameters with seed = seed_str } }

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

let build_log sched rating =
  {
    rating;
    state = sched.current.state;
    elapsed_days = sched.current.elapsed_days;
    scheduled_days = sched.current.scheduled_days;
    reviewed_date = sched.now;
  }

module type Scheduler = sig
  val preview : t -> record_log
  val review : t -> rating -> (t * scheduling_info)
end

let mk_next_map sched (next_again, next_hard, next_good, next_easy) =
  let again_scheduling_info = { card = next_again; review_log = build_log sched Again } in
  let hard_scheduling_info = { card = next_hard; review_log = build_log sched Hard } in
  let good_scheduling_info = { card = next_good; review_log = build_log sched Good } in
  let easy_scheduling_info = { card = next_easy; review_log = build_log sched Easy } in
  let next' = RatingMap.add Again again_scheduling_info sched.next in
  let next'' = RatingMap.add Hard hard_scheduling_info next' in
  let next''' = RatingMap.add Good good_scheduling_info next'' in
  let next'''' = RatingMap.add Easy easy_scheduling_info next''' in  
  next''''
