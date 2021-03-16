open OUnit2
open Trs_bridge
let test_parse_ctrls_1 _ =
  let expected_ctrl_1 = "A:0" |> Bigraph.Ctrl.of_string
  and expected_ctrl_2 = "B:0" |> Bigraph.Ctrl.of_string
  in
  let expected_ctrls = [expected_ctrl_1;expected_ctrl_2]
  and parsed_ctrls = Parsing.parse_ctrls "ctrls.csv"
  in
  assert_equal ~msg:"Parsed ctrls are not equal to expected" expected_ctrls parsed_ctrls
let test_parse_react_times_1 _ =
  let expected_react_times = Hashtbl.create 3 
  and parsed_react_times = Parsing.parse_react_times "t2ts.csv" in
  let _ = Hashtbl.add expected_react_times "r1" 1;Hashtbl.add expected_react_times "r2" 3;Hashtbl.add expected_react_times "r3" 1
  in
  assert_equal ~msg:"Parsed react times are not equal to expected" expected_react_times parsed_react_times
let suite =
  "Parsing tests" >::: [
    "Parsing ctrls test 1">:: test_parse_ctrls_1;
    "Parsing react times test 1">:: test_parse_react_times_1
]

let () =
  run_test_tt_main suite