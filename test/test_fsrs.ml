let test_update_card () = ()

let () =
  let open Alcotest in
  run "FSRS" [
    "update_card", [ test_case "dummy test" `Quick test_update_card ];
  ]
