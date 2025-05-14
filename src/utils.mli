val span_to_dhms : Timedesc.Span.t -> int * int * int * int

val span_to_days: Timedesc.Span.t -> int
val span_to_hours: Timedesc.Span.t -> int
val span_to_minutes: Timedesc.Span.t -> int
val span_to_seconds: Timedesc.Span.t -> int

val clamp_float: float -> float -> float -> float

val mk_minutes : int -> Timedesc.timestamp
val mk_days : int -> Timedesc.timestamp