open Models

let () = 
  let params = Parameters.default () in
  let fsrs = Fsrs.create params in
  let card = Models.new_card () in
  
  let record_log = Fsrs.repeat fsrs card (Timedesc.Timestamp.now ()) in
  Models.RatingMap.iter (fun rating sched_info ->
    Printf.printf "If you answer \"%s\":\n" (Models.show_rating rating);
    Printf.printf "  Next card: %s\n" (Models.show_card sched_info.card);
    Printf.printf "  Review log: %s\n" (Models.show_review_log sched_info.review_log);
    print_endline ""
  ) record_log