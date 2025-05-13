let span_to_dhms span =
  let total_seconds = int_of_float (Timedesc.Span.to_float_s span) in
  let days = total_seconds / (24 * 3600) in
  let hours = (total_seconds mod (24 * 3600)) / 3600 in
  let minutes = (total_seconds mod 3600) / 60 in
  let seconds = total_seconds mod 60 in
  (days, hours, minutes, seconds)

let span_to_days span =
  let total_seconds = int_of_float (Timedesc.Span.to_float_s span) in
  total_seconds / (24 * 3600)

let span_to_hours span =
  let total_seconds = int_of_float (Timedesc.Span.to_float_s span) in
  total_seconds / 3600

let span_to_minutes span =
  let total_seconds = int_of_float (Timedesc.Span.to_float_s span) in
  total_seconds / 60

let span_to_seconds span =
  let total_seconds = int_of_float (Timedesc.Span.to_float_s span) in
  total_seconds


let clamp_float (lo : float) (hi : float) (x : float) : float = min hi (max lo x)
