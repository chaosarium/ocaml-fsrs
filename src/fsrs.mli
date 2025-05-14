(* Main interface to the FSRS scheduler *)

open Models

type t = {
  parameters : Parameters.t;
}

val create : Parameters.t -> t

val scheduler : t -> card -> ts -> (module Scheduler.Scheduler) * Scheduler.t

val repeat : t -> card -> ts -> record_log

val next : t -> card -> ts -> rating -> scheduling_info