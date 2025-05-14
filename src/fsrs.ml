open Models
open Scheduler
open Scheduler_basic
(* open Scheduler_longterm *)

type t = {
  parameters : Parameters.t;
}

let create params = { parameters = params }

let scheduler (fsrs : t) (card : card) (now : Timedesc.Timestamp.t) : (module Scheduler.Scheduler) * Scheduler.t =
  if fsrs.parameters.enable_short_term then
    let sched = Scheduler.create fsrs.parameters card now in
    ((module Scheduler_basic : Scheduler.Scheduler), sched)
  else
    (* let sched = Scheduler_longterm.create fsrs.parameters card now in
    ((module Scheduler_longterm : Scheduler.Scheduler), sched) *)
    failwith "not yet implemented"

let repeat (fsrs : t) (card : card) (now : Timedesc.Timestamp.t) : record_log =
  let (module S : Scheduler.Scheduler), sched = scheduler fsrs card now in
  S.preview sched card

let next (fsrs : t) (card : card) (now : Timedesc.Timestamp.t) (rating : rating) : scheduling_info =
  let (module S : Scheduler.Scheduler), sched = scheduler fsrs card now in
  let _, info = S.review sched card rating in
  info
