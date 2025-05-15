open Alcotest
open Models
open Alea

let _TEST_RATINGS = [|
  Good; Good; Good; Good; Good; Good; Again; Again; Good; Good; Good; Good; Good
|]

let _WEIGHTS = [|
  0.4197; 1.1869; 3.0412; 15.2441; 7.1434; 0.6477; 1.0007; 0.0674; 1.6597; 0.1712; 1.1178; 2.0225; 0.0904; 0.3025; 2.1214; 0.2498; 2.9466; 0.4891; 0.6468;
|]

let string_to_utc (s : string) : ts =
  match Timedesc.Timestamp.of_iso8601 s with
    | Ok ts -> ts
    | Error msg -> failwith ("Failed to parse timestamp: " ^ msg)
  
let state_testable =
  Alcotest.testable
    (fun fmt s -> Format.pp_print_string fmt (show_state s))
    ( = )

let card_testable = 
  Alcotest.testable
    (fun fmt c -> Format.pp_print_string fmt (show_card c))
    ( = )

(* basic tests *)

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

(* longterm tests *)

let test_long_term_scheduler () =
  let params = { (Parameters.default ()) with w = _WEIGHTS; enable_short_term = false } in
  let fsrs = Fsrs.create params in
  let card = Models.new_card () in
  let now = string_to_utc "2022-11-29T12:30:00Z" in
  let interval_history', stability_history', difficulty_history', _, _ =
    Array.fold_left
      (fun (intervals, stabilities, difficulties, card, now) rating ->
        let record_log = Fsrs.repeat fsrs card now in
        let info = RatingMap.find rating record_log in
        let next = Fsrs.next fsrs card now rating in

        check card_testable "card equality" info.card next.card;
        let card' = info.card in
        let now' = card'.due in
        
        ( card'.scheduled_days :: intervals,
          card'.stability :: stabilities,
          card'.difficulty :: difficulties,
          card', now'
        )
      )
      ([], [], [], card, now)
      _TEST_RATINGS
  in
  let intervals = Array.of_list (List.rev interval_history') in
  let stabilities = Array.of_list (List.rev stability_history') in
  let difficulties = Array.of_list (List.rev difficulty_history') in
  let expected_intervals = [| 3; 13; 48; 155; 445; 1158; 17; 3; 9; 27; 74; 190; 457 |] in
  let expected_stabilities = [| 3.0412; 13.0913; 48.1585; 154.9373; 445.0556; 1158.0778; 16.6306; 2.9888; 9.4633; 26.9474; 73.9723; 189.7037; 457.4379 |] in
  let expected_difficulties = [| 4.4909; 4.2666; 4.0575; 3.8624; 3.6804; 3.5108; 5.219; 6.8122; 6.4314; 6.0763; 5.7452; 5.4363; 5.1483 |] in
  check (array int) "interval history" expected_intervals intervals;
  check (array (float 0.0001)) "stability history" expected_stabilities stabilities;
  check (array (float 0.0001)) "difficulty history" expected_difficulties difficulties;
  ()

(* alea tests *)

let alea_state_testable =
  Alcotest.testable
    (fun fmt (s : Alea.alea_t) ->
      Format.fprintf fmt "{ c=%g; s0=%g; s1=%g; s2=%g }" s.c s.s0 s.s1 s.s2
    )
    ( = )

let test_prng_get_state () =
  let prng_1 = prng_new (String "1") in
  let prng_2 = prng_new (String "2") in
  let prng_3 = prng_new (String "1") in
  let alea_state_1 = prng_get_state prng_1 in
  let alea_state_2 = prng_get_state prng_2 in
  let alea_state_3 = prng_get_state prng_3 in
  check alea_state_testable "alea_state_1 = alea_state_3" alea_state_1 alea_state_3;
  check bool "alea_state_1 <> alea_state_2" true (alea_state_1 <> alea_state_2)

let test_alea_get_next () =
  let prng = prng_new (String "12345") in
  let v1 = prng_gen_next prng in
  let v2 = prng_gen_next prng in
  let v3 = prng_gen_next prng in
  check (float 1e-15) "gen_next 1" 0.27138191112317145 v1;
  check (float 1e-15) "gen_next 2" 0.19615925149992108 v2;
  check (float 1e-15) "gen_next 3" 0.6810678059700876 v3

let test_alea_int32 () =
  let prng = prng_new (String "12345") in
  let i1 = prng_int32 prng in
  let i2 = prng_int32 prng in
  let i3 = prng_int32 prng in
  check int "int32 1" 1165576433 i1;
  check int "int32 2" 842497570 i2;
  check int "int32 3" (-1369803343) i3

let test_alea_import_state () =
  let prng_1 = prng_new (String (string_of_int (int_of_float ((Random.float 1.0) *. 2147483647.)))) in
  let _ = (prng_gen_next prng_1) in
  let _ = (prng_gen_next prng_1) in
  let _ = (prng_gen_next prng_1) in
  let prng_1_state = prng_get_state prng_1 in
  let prng_2 = prng_import_state (prng_new Empty) prng_1_state in
  check alea_state_testable "imported state equality" (prng_get_state prng_1) (prng_get_state prng_2);
  for _ = 1 to 10_000 do
    let a = prng_gen_next prng_1 in
    let b = prng_gen_next prng_2 in
    check (float 1e-15) "imported stream equality" a b;
    check bool "a in [0,1)" true (a >= 0.0 && a < 1.0);
    check bool "b in [0,1)" true (b >= 0.0 && b < 1.0);
  done

let test_seed_example_1 () =
  let prng = prng_new (String "1727015666066") in
  let results = prng_gen_next prng in
  let state = prng_get_state prng in
  let expect_alea_state = { c = 1828249.0; s0 = 0.5888567129150033; s1 = 0.5074866858776659; s2 = 0.6320083506871015 } in
  check (float 1e-15) "gen_next" 0.6320083506871015 results;
  check alea_state_testable "alea_state" expect_alea_state state

let test_seed_example_2 () =
  let prng = prng_new (String "Seedp5fxh9kf4r0") in
  let results = prng_gen_next prng in
  let state = prng_get_state prng in
  let expect_alea_state = { c = 1776946.0; s0 = 0.6778371171094477; s1 = 0.0770602801349014; s2 = 0.14867847645655274 } in
  check (float 1e-15) "gen_next" 0.14867847645655274 results;
  check alea_state_testable "alea_state" expect_alea_state state

let test_seed_example_3 () =
  let prng = prng_new (String "NegativeS2Seed") in
  let results = prng_gen_next prng in
  let state = prng_get_state prng in
  let expect_alea_state = { c = 952982.0; s0 = 0.25224833423271775; s1 = 0.9213257452938706; s2 = 0.830770346801728 } in
  check (float 1e-15) "gen_next" 0.830770346801728 results;
  check alea_state_testable "alea_state" expect_alea_state state

(* retrievability test *)

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
    "test_longterm_scheduler", [
      test_case "test_long_term_scheduler" `Quick test_long_term_scheduler;
    ];
    "test_get_retrievability", [
      test_case "test_get_retrievability" `Quick test_get_retrievability;
    ];
    "test_alea", [
      test_case "test_prng_get_state" `Quick test_prng_get_state;
      test_case "test_alea_get_next" `Quick test_alea_get_next;
      test_case "test_alea_int32" `Quick test_alea_int32;
      test_case "test_alea_import_state" `Quick test_alea_import_state;
      test_case "test_seed_example_1" `Quick test_seed_example_1;
      test_case "test_seed_example_2" `Quick test_seed_example_2;
      test_case "test_seed_example_3" `Quick test_seed_example_3;
    ];
  ]
