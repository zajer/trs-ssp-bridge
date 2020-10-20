open OUnit2
module IntMap = Map.Make(Int);;
open Trs_bridge
let id_iso_test_1 _ = 
  let dom = [1;2;3;4] in
  let expected_iso = Bigraph.Iso.of_list [(1,1);(2,2);(3,3);(4,4)]
  and gen_iso = Common.make_id_iso dom in
  assert_equal expected_iso gen_iso
let id_iso_test_2 _ = 
  let dom = [1;3;7;15] in
  let expected_iso = Bigraph.Iso.of_list [(1,1);(3,3);(7,7);(15,15)]
  and gen_iso = Common.make_id_iso dom in
  assert_equal expected_iso gen_iso
let suite =
  "Normalization" >::: [
    "Identity iso generation test 1">:: id_iso_test_1;
    "Identity iso generation test 2">:: id_iso_test_2;
    
]

let () =
  run_test_tt_main suite
