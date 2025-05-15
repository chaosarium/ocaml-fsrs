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

let string_to_utc (s : string) : ts =
  match Timedesc.Timestamp.of_iso8601 s with
    | Ok ts -> ts
    | Error msg -> failwith ("Failed to parse timestamp: " ^ msg)
  
let state_testable =
  Alcotest.testable
    (fun fmt s -> Format.pp_print_string fmt (show_state s))
    ( = )

let test_basic_scheduler_interval () =
  let params = Parameters.default () in
  let fsrs = Fsrs.create params in
  let card = Models.new_card () in
  let now = string_to_utc "2022-11-29T12:30:00Z" in
  let interval_list', _, _ =
    Array.fold_left
      (fun (acc, card, now) rating ->
        let info = Fsrs.next fsrs card now rating in
        let card' = info.card in
        let now' = card'.due in
        (card'.scheduled_days::acc, card', now'))
      ([], card, now)
      _TEST_RATINGS
  in
  let intervals = Array.of_list (List.rev interval_list') in
  let expected_intervals = [| 0; 4; 15; 48; 136; 351; 0; 0; 7; 13; 24; 43; 77 |] in
  check (array int) "scheduled_days history" expected_intervals intervals

let test_basic_scheduler_state () =
  let params = { (Parameters.default ()) with w = _WEIGHTS } in
  let fsrs = Fsrs.create params in
  let card = Models.new_card () in
  let now = string_to_utc "2022-11-29T12:30:00Z" in
  let state_list', _, _ =
    Array.fold_left
      (fun (acc, card, now) rating ->
        let record_log = Fsrs.repeat fsrs card now in
        let info = RatingMap.find rating record_log in
        let card' = info.card in
        let rev_log = info.review_log in
        let now' = card'.due in
        (rev_log.state :: acc, card', now'))
      ([], card, now)
      _TEST_RATINGS
  in
  let states = Array.of_list (List.rev state_list') in
  let expected = [|
    New; Learning; Review; Review; Review; Review; Review; Relearning; Relearning; Review;
    Review; Review; Review
  |] in
  check (array state_testable) "state history" expected states

let test_basic_scheduler_memo_state () =
  let params = { (Parameters.default ()) with w = _WEIGHTS } in
  let fsrs = Fsrs.create params in
  let card = Models.new_card () in
  let now = string_to_utc "2022-11-29T12:30:00Z" in
  let rating_interval_s = [|
    (0, Again); 
    (0, Good); 
    (1, Good); 
    (3, Good); 
    (8, Good); 
    (21, Good);
  |] in
  let record_log_list', last_card, last_now =
    Array.fold_left
      (fun (acc, card, now) (interval, rating) ->
        let record_log = Fsrs.repeat fsrs card now in
        let info = RatingMap.find rating record_log in
        let card' = info.card in
        let rev_log = info.review_log in
        let now' = (Timedesc.Span.add now (Utils.mk_days interval)) in
        (record_log :: acc, card', now'))
      ([], card, now)
      rating_interval_s
  in
  let last_record_log = List.hd record_log_list' in
  let final_record_log = Fsrs.repeat fsrs last_card last_now in
  let final_info = RatingMap.find Good final_record_log in
  let final_card = final_info.card in
  check (float 0.0001) "difficulty" 5.0976 (final_card.difficulty);
  check (float 0.0001) "stability" 71.4554 (final_card.stability);
  ()

let test_get_retrievability () =
  let params = Parameters.default () in
  let fsrs = Fsrs.create params in
  let card = Models.new_card () in
  let now = string_to_utc "2022-11-29T12:30:00Z" in
  let expect_retrievability = [| 1.0; 1.0; 1.0; 0.9026208 |] in
  let record_log = Fsrs.repeat fsrs card now in
  List.iteri
    (fun i rating ->
      let info = RatingMap.find rating record_log in
      let card' = info.card in
      let retrievability = Models.get_retrievability card' ~now:card'.due (Parameters.forgetting_curve fsrs.parameters) in
      check (float 0.0000001) "retrievability" expect_retrievability.(i) retrievability
    )
    Models.possible_ratings

let () =
  let open Alcotest in
  run "FSRS Scheduler" [
    "test_basic_scheduler", [ 
      test_case "test_basic_scheduler_interval" `Quick test_basic_scheduler_interval;
      test_case "test_basic_scheduler_state" `Quick test_basic_scheduler_state;
      test_case "test_basic_scheduler_memo_state" `Quick test_basic_scheduler_memo_state;
    ];
    "test_get_retrievability", [
      test_case "test_get_retrievability" `Quick test_get_retrievability;
    ]
  ]
