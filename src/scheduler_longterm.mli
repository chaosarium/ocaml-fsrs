open Models

(* return a map from possible ratings to the new schedule the schedule will result in *)
val preview : Scheduler.t -> card -> record_log

(* take a rating and update the card and the new scheduling state *)
val review : Scheduler.t -> card -> rating -> (card * scheduling_info)
