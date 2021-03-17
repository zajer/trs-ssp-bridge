open OUnit2
module IntMap = Map.Make(Int)
open Trs_bridge
let test_export_dest_states_1 _ =
  let tmp_file = "tmp_export_dest_states_1.csv" in
  let exported_dest_state_1 = { Patterns.state_idx=7;patts_found=["yolo";"swag"] }
  and exported_dest_state_2 = { Patterns.state_idx=21;patts_found=["bilbo";"baggins"] } in
  Patterns.export_dest_state [exported_dest_state_1;exported_dest_state_2] tmp_file;
  let exported_dst_states = Csv.load tmp_file in
  assert_bool "There should be two rows in exported file" (List.length exported_dst_states = 2);
  assert_equal 
    ~msg:"First row of exported dst states is not equal to expected"
    ~printer:(fun sl -> assert (List.length sl = 2) ; "idx:"^(List.nth sl 0)^" patterns:"^(List.nth sl 1)  ) 
    ["7";"[yolo;swag]"] (List.nth exported_dst_states 0);
    assert_equal 
    ~msg:"The second row of exported dst states is not equal to expected"
    ~printer:(fun sl -> assert (List.length sl = 2) ; "idx:"^(List.nth sl 0)^" patterns:"^(List.nth sl 1)  ) 
    ["21";"[bilbo;baggins]"] (List.nth exported_dst_states 1) 
let _destination_state_2_string ds =
  let state_idx = string_of_int ds.Patterns.state_idx
          and descriptions = "["^(String.concat ";" ds.patts_found)^"]"
          in
          "{"^state_idx^" ;"^descriptions^"}"
let _are_dest_states_equal ds1 ds2 =
  let res = ds1.Patterns.state_idx = ds2.Patterns.state_idx &&
  List.for_all (fun ds1e -> List.exists (fun ds2e -> ds1e = ds2e) ds2.patts_found) ds1.patts_found in
  res
let test_import_dest_states_1 _ =
  let expected_dest_state_1 = { Patterns.state_idx=7;patts_found=["yolo";"swag"] }
  and expected_dest_state_2 = { Patterns.state_idx=21;patts_found=["bilbo";"baggins"] }
  and source_file = "dest_states.csv"
  in
  let imported_dest_states = Patterns.import_dest_states source_file in
  assert_equal 
    ~msg:"There should be exactly two imported destination states"
    2
    (List.length imported_dest_states);
  assert_equal
    ~msg:"Imported destinations states do not match with the expected"
    ~cmp:
      (
        fun dsl1 dsl2 -> 
          List.for_all (fun dsl1e -> List.exists (fun dsl2e -> _are_dest_states_equal dsl1e dsl2e) dsl2) dsl1
      )
    ~printer:
      (
        fun dsl ->
          let dsl_sl = List.map (fun ds -> _destination_state_2_string ds) dsl in
          String.concat ";" dsl_sl
      )
      [expected_dest_state_1;expected_dest_state_2]
      imported_dest_states
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
    "Test 1">:: patt_detection_1;
    "Destination states export test 1">::test_export_dest_states_1;
    "Destination states import test 1">::test_import_dest_states_1;
]

let () =
  run_test_tt_main suite
