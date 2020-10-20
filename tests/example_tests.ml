open OUnit2

let test_1 _ = 
    assert_equal true false
let suite =
  "My tests" >::: [
    "My test 1">:: test_1;
]

let () =
  run_test_tt_main suite
