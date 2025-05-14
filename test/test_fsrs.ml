open Alcotest
open Models

let _TEST_RATINGS = [|
  Good; Good; Good; Good; Good; Good;
  Again; Again;
  Good; Good; Good; Good; Good
|]

let _WEIGHTS = [|
    0.4197; 1.1869; 3.0412; 15.2441; 7.1434; 0.6477; 1.0007; 0.0674; 1.6597; 0.1712; 1.1178;
    2.0225; 0.0904; 0.3025; 2.1214; 0.2498; 2.9466; 0.4891; 0.6468;
|]

let string_to_utc str =
  match Timedesc.Timestamp.of_iso8601 str with
    | Ok ts -> ts
    | Error msg -> failwith ("Failed to parse timestamp: " ^ msg)

(* FOR LATER *)
(* let params = {(Parameters.default ()) with w = _WEIGHTS} in *)
  
let test_basic_scheduler_interval () =
  let params = Parameters.default () in
  let fsrs = Fsrs.create params in
  let card = Models.new_card () in
  let now = string_to_utc "2022-11-29T12:30:00Z" in
  let intervals, _, _ =
    Array.fold_left
      (fun (acc, card, now) rating ->
        let info = Fsrs.next fsrs card now rating in
        let card' = info.card in
        let now' = card'.due in
        (card'.scheduled_days::acc, card', now'))
      ([], card, now)
      _TEST_RATINGS
  in
  let intervals = Array.of_list (List.rev intervals) in
  let expected_intervals = [| 0; 4; 15; 48; 136; 351; 0; 0; 7; 13; 24; 43; 77 |] in
  check (array int) "scheduled_days history" expected_intervals intervals


let () =
  let open Alcotest in
  run "FSRS Scheduler" [
    "test_basic_scheduler_interval", [ test_case "test_basic_scheduler_interval" `Quick test_basic_scheduler_interval ]
  ]
