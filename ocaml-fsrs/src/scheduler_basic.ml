open Models
open Scheduler

(* used for new cards. unit init S and D before the card's learnt *)
let new_state (sched : t) (rating : rating) : (t * scheduling_info) = 
  try 
    let res = RatingMap.find rating sched.next in (sched, res)
  with Not_found ->
    let next_difficulty = Parameters.init_difficulty sched.parameters ~rating in
    let next_stability = Parameters.init_stability sched.parameters ~rating in
    let next = match rating with
      | Again -> {sched.current with 
          scheduled_days = 0;
          due = Timedesc.Timestamp.add sched.now (Utils.mk_minutes 1);
          state = Learning;
          difficulty = next_difficulty;
          stability = next_stability;
        }
      | Hard -> {sched.current with 
          scheduled_days = 0;
          due = Timedesc.Timestamp.add sched.now (Utils.mk_minutes 5);
          state = Learning;
          difficulty = next_difficulty;
          stability = next_stability;
        }
      | Good -> {sched.current with 
          scheduled_days = 0;
          due = Timedesc.Timestamp.add sched.now (Utils.mk_minutes 10);
          state = Learning;
          difficulty = next_difficulty;
          stability = next_stability;
        }
      | Easy -> 
        let easy_interval = Parameters.next_interval sched.parameters ~stability:sched.current.stability ~elapsed_days:sched.current.elapsed_days in
        {sched.current with 
          scheduled_days = int_of_float easy_interval;
          due = Timedesc.Timestamp.add sched.now (Utils.mk_days (int_of_float easy_interval));
          state = Review;
          difficulty = next_difficulty;
          stability = next_stability;
        }
    in
    let res : scheduling_info = { card = next; review_log = build_log sched rating } in 
    ({sched with next = RatingMap.add rating res sched.next}, res)

(* used for cards in learning state *)
let learning_state (sched : t) (rating : rating) : (t * scheduling_info) = 
  try 
    let res = RatingMap.find rating sched.next in (sched, res)
  with Not_found ->
    let next_difficulty = Parameters.next_difficulty sched.parameters ~difficulty:sched.last.difficulty ~rating in
    let next_stability = Parameters.short_term_stability sched.parameters ~stability:sched.last.stability ~rating in
    let next = match rating with
      | Again -> {sched.current with 
          scheduled_days = 0;
          due = Timedesc.Timestamp.add sched.now (Utils.mk_minutes 5);
          state = sched.last.state;
          difficulty = next_difficulty;
          stability = next_stability;
        }
      | Hard -> {sched.current with 
          scheduled_days = 0;
          due = Timedesc.Timestamp.add sched.now (Utils.mk_minutes 10);
          state = sched.last.state;
          difficulty = next_difficulty;
          stability = next_stability;
        }
      | Good -> 
        let good_interval = Parameters.next_interval sched.parameters ~stability:sched.current.stability ~elapsed_days:sched.current.elapsed_days in
        {sched.current with 
          scheduled_days = int_of_float good_interval;
          due = Timedesc.Timestamp.add sched.now (Utils.mk_days (int_of_float good_interval));
          state = Review;
          difficulty = next_difficulty;
          stability = next_stability;
        }
      | Easy -> 
        let good_stability = Parameters.short_term_stability sched.parameters ~stability:sched.last.stability ~rating:Good in
        let good_interval = Parameters.next_interval sched.parameters ~stability:good_stability ~elapsed_days:sched.current.elapsed_days in
        let easy_interval = 
          let unclamped = Parameters.next_interval sched.parameters ~stability:next_stability ~elapsed_days:sched.current.elapsed_days in
          max unclamped (good_interval +. 1.0)
        in
        {sched.current with 
          scheduled_days = int_of_float easy_interval;
          due = Timedesc.Timestamp.add sched.now (Utils.mk_days (int_of_float easy_interval));
          state = Review;
          difficulty = next_difficulty;
          stability = next_stability;
        }
    in
    let res : scheduling_info = { card = next; review_log = build_log sched rating } in 
    ({sched with next = RatingMap.add rating res sched.next}, res)

let review_state (sched : t) (rating : rating) : (t * scheduling_info) = 
  try 
    let res = RatingMap.find rating sched.next in (sched, res)
  with Not_found ->
    let interval = sched.current.elapsed_days in
    let stability = sched.last.stability in
    let difficulty = sched.last.difficulty in
    let retrievability = get_retrievability sched.last ~now:sched.now (Parameters.forgetting_curve sched.parameters) in
    
    let next_forget_stability = Parameters.next_forget_stability sched.parameters ~difficulty ~stability ~retrievability in
    let next_hard_stability = Parameters.next_recall_stability sched.parameters ~difficulty ~stability ~retrievability ~rating:Hard in
    let next_good_stability = Parameters.next_recall_stability sched.parameters ~difficulty ~stability ~retrievability ~rating:Good in
    let next_easy_stability = Parameters.next_recall_stability sched.parameters ~difficulty ~stability ~retrievability ~rating:Easy in
    
    let hard_interval = Parameters.next_interval sched.parameters ~stability:next_hard_stability ~elapsed_days:interval in
    let good_interval = Parameters.next_interval sched.parameters ~stability:next_good_stability ~elapsed_days:interval in
    let hard_interval = min hard_interval good_interval in
    let good_interval = max good_interval (hard_interval +. 1.0) in
    let easy_interval = max (Parameters.next_interval sched.parameters ~stability:next_easy_stability ~elapsed_days:interval) (good_interval +. 1.0) in
    
    let next_again = 
      let next_interval = 1.5 in
      {sched.current with 
        scheduled_days = int_of_float next_interval;
        due = Timedesc.Timestamp.add sched.now (Utils.mk_days (int_of_float next_interval));
        state = Relearning;
        difficulty = Parameters.init_difficulty sched.parameters ~rating:Again;
        stability = next_forget_stability;
      } 
    in
    let next_hard = 
      let next_interval = 1.5 in
      {sched.current with 
        scheduled_days = int_of_float next_interval;
        due = Timedesc.Timestamp.add sched.now (Utils.mk_days (int_of_float next_interval));
        state = Review;
        difficulty = Parameters.init_difficulty sched.parameters ~rating:Hard;
        stability = next_hard_stability;
      } 
    in
    let next_good = 
      let next_interval = 1.5 in
      {sched.current with 
        scheduled_days = int_of_float next_interval;
        due = Timedesc.Timestamp.add sched.now (Utils.mk_days (int_of_float next_interval));
        state = Review;
        difficulty = Parameters.init_difficulty sched.parameters ~rating:Good;
        stability = next_good_stability;
      } 
    in
    let next_easy = 
      let next_interval = 1.5 in
      {sched.current with 
        scheduled_days = int_of_float next_interval;
        due = Timedesc.Timestamp.add sched.now (Utils.mk_days (int_of_float next_interval));
        state = Review;
        difficulty = Parameters.init_difficulty sched.parameters ~rating:Easy;
        stability = next_easy_stability;
      } 
    in
    
    let again_scheduling_info = { card = next_again; review_log = build_log sched Again } in
    let hard_scheduling_info = { card = next_hard; review_log = build_log sched Hard } in
    let good_scheduling_info = { card = next_good; review_log = build_log sched Good } in
    let easy_scheduling_info = { card = next_easy; review_log = build_log sched Easy } in
    
    let next' = RatingMap.add Again again_scheduling_info sched.next in
    let next'' = RatingMap.add Hard hard_scheduling_info next' in
    let next''' = RatingMap.add Good good_scheduling_info next'' in
    let next'''' = RatingMap.add Easy easy_scheduling_info next''' in  
    
    ({sched with next = next''''}, RatingMap.find rating next'''')

let preview (sched : t) (card : card) : record_log =
  let now = sched.now in
  RatingMap.empty

let review (sched : t) (card : card) (rating : rating) : (card * scheduling_info) =
  let now = sched.now in
  let elapsed_days = card.elapsed_days + 1 in
  let scheduled_days = card.scheduled_days + 1 in
  let new_card = { card with elapsed_days; scheduled_days } in
  let review_log = build_log sched rating in
  let info = { card = new_card; review_log } in
  (new_card, info)