(*! defines scheduler interface and scheduler signature *)

open Models

type t = {
  parameters : Parameters.t;
  last : card;
  current : card;
  now : Timedesc.Timestamp.t;
  next : record_log;
}

val create : Parameters.t -> card -> Timedesc.Timestamp.t -> t

(* log entry of this review *)
val build_log : t -> rating -> review_log

(* return new cheduler with newly initialised seed *)
val init_seed : t -> t

module type Scheduler = sig
  val preview : t -> record_log
  val review : t -> rating -> (t * scheduling_info)
end

val mk_next_map : t -> card * card * card * card -> record_log