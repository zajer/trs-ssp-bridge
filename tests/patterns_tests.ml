open OUnit2
module IntMap = Map.Make(Int)
open Trs_bridge
let patt_detection_1 _ = 
  let b0 = "{(0, AT1:3),(1, A:0),(2, B:0),(3, AT1:3),(4, AT1:3),(5, AT2:1)}\n1 6 0\n100111\n011000\n000000\n000000\n000000\n000000\n000000\n({}, {ca1a2}, {(0, 1), (3, 1)})\n({}, {ca2a3}, {(3, 1), (4, 1)})\n({}, {ca2a4}, {(3, 1), (5, 1)})\n({}, {lc1a1}, {(0, 1)})\n({}, {lc1a3}, {(4, 1)})\n({}, {lc2a1}, {(0, 1)})\n({}, {lc2a3}, {(4, 1)})"
  and b1 = "{(0, AT1:3),(1, AT1:3),(2, A:0),(3, B:0),(4, AT1:3),(5, AT2:1)}\n1 6 0\n110011\n000000\n000000\n000000\n000000\n000000\n001100\n({}, {ca1a2}, {(0, 1), (1, 1)})\n({}, {ca2a3}, {(1, 1), (4, 1)})\n({}, {ca2a4}, {(1, 1), (5, 1)})\n({}, {lc1a1}, {(0, 1)})\n({}, {lc1a3}, {(4, 1)})\n({}, {lc2a1}, {(0, 1)})\n({}, {lc2a3}, {(4, 1)})"
  and b1_iso = "{(0, AT1:3),(1, AT1:3),(2, B:0),(3, A:0),(4, AT1:3),(5, AT2:1)}\n1 6 0\n110011\n000000\n000000\n000000\n000000\n000000\n001100\n({}, {ca1a2}, {(0, 1), (1, 1)})\n({}, {ca2a3}, {(1, 1), (4, 1)})\n({}, {ca2a4}, {(1, 1), (5, 1)})\n({}, {lc1a1}, {(0, 1)})\n({}, {lc1a3}, {(4, 1)})\n({}, {lc2a1}, {(0, 1)})\n({}, {lc2a3}, {(4, 1)})"
  and patt = "{(0, AT2:1),(1, A:0),(2, B:0)}\n1 3 0\n100\n011\n000\n000\n({}, {x}, {(0, 1)})"
  in
  let s_parsed = [(0,Bigraph.Big.of_string b0);(1,Bigraph.Big.of_string b1);(2,Bigraph.Big.of_string b1_iso)]
  and lop = [Bigraph.Big.of_string patt] in
  let found_patts = Patterns.find_destination_states s_parsed lop in
  assert_equal (List.length found_patts) 2 ;
  assert_equal (List.nth found_patts 0) 1 ;
  assert_equal (List.nth found_patts 1) 2 

let suite =
  "Pattern detection" >::: [
    "Test 1">:: patt_detection_1
]

let () =
  run_test_tt_main suite
