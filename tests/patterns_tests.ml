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
let test_pattern_detection_1 _ = 
  let b0 = "{(0, A:0),(1, B:0),(2, C:0),(3, D:0)}\n0 4 0\n0110\n0001\n0000\n0000"
  and b1 = "{(0, A:0),(1, B:0),(2, C:0),(3, D:0)}\n0 4 0\n0110\n0000\n0001\n0000"
  and b1_iso = "{(0, D:0),(1, C:0),(2, B:0),(3, A:0)}\n0 4 0\n0000\n1000\n0000\n0110"
  and patt = "{(0, C:0),(1, D:0)}\n1 2 0\n10\n01\n00"
  in
  let states_parsed = 
    [
      {Tracking_bigraph.TTS.bigraph=Bigraph.Big.of_string b0;index=0};
      {Tracking_bigraph.TTS.bigraph=Bigraph.Big.of_string b1;index=1};
      {Tracking_bigraph.TTS.bigraph=Bigraph.Big.of_string b1_iso;index=2};
    ]
  and lop = [{Patterns.bigraph=Bigraph.Big.of_string patt;description="my pattern"}] in
  let found_ds = Patterns.find_dest_states states_parsed lop 
  and expected_ds = 
    [
      {Patterns.state_idx=1;patts_found=["my pattern"]};
      {Patterns.state_idx=2;patts_found=["my pattern"]}
    ]
  in
  assert_equal 
    ~msg:"There should be two destination states found among parsed states" 
    ~printer:(fun length -> string_of_int length)
    (List.length expected_ds) (List.length found_ds) ;
  assert_equal 
    ~msg:"Destination states not equal to expected" 
    ~printer:(fun dsl-> List.map (fun ds -> _destination_state_2_string ds) dsl |> String.concat ";" )
    expected_ds 
    found_ds
  
let suite =
  "Patterns" >::: [
    "Destination states export test 1">::test_export_dest_states_1;
    "Destination states import test 1">::test_import_dest_states_1;
    "Pattern detection test 1">::test_pattern_detection_1
]

let () =
  run_test_tt_main suite
