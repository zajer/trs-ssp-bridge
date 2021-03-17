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
  
let test_parse_patterns_1 _ =
  let patterns_file = "patterns.csv"
  and pattern_1_big = "{(0, AT2:1),(1, A:0),(2, B:0)}\n1 3 0\n100\n011\n000\n000\n({}, {x}, {(0, 1)})" |> Bigraph.Big.of_string
  and pattern_2_big = "{(0, AT1:3),(1, AT1:3),(2, B:0),(3, A:0),(4, AT1:3),(5, AT2:1)}\n1 6 0\n110011\n000000\n000000\n000000\n000000\n000000\n001100\n({}, {ca1a2}, {(0, 1), (1, 1)})\n({}, {ca2a3}, {(1, 1), (4, 1)})\n({}, {ca2a4}, {(1, 1), (5, 1)})\n({}, {lc1a1}, {(0, 1)})\n({}, {lc1a3}, {(4, 1)})\n({}, {lc2a1}, {(0, 1)})\n({}, {lc2a3}, {(4, 1)})" |> Bigraph.Big.of_string in
  let expected_pattern_1 = {Patterns.bigraph=pattern_1_big;description="yolo"}
  and expected_pattern_2 = {Patterns.bigraph=pattern_2_big;description="swag"} in
  let expected_patterns = [expected_pattern_1;expected_pattern_2]
  and imported_patterns = Parsing.parse_destingation_patterns patterns_file in
  assert_bool "Number of imported patterns differs from the number of expected patterns" (List.length expected_patterns = List.length imported_patterns);
  assert_equal 
    ~msg:"Imported patterns are not equal to expected"
    expected_patterns
    imported_patterns
  
let suite =
  "Parsing tests" >::: [
    "Parsing ctrls test 1">:: test_parse_ctrls_1;
    "Parsing react times test 1">:: test_parse_react_times_1;
    "Parsing destination state patterns test 1">:: test_parse_patterns_1
]

let () =
  run_test_tt_main suite