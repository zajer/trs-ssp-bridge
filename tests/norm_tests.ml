open OUnit2
open Ssp_bridge
let test_norm_single_transition_1 _ = 
  let b0_template = Norm_tests_data.test_norm_single_transition_1_b0_str
  and b1_template = Norm_tests_data.test_norm_single_transition_1_b1_str
  and b0_iso = Norm_tests_data.test_norm_single_transition_1_b0_iso_str
  and reference_in_state_idx = 1
  and reference_out_state_idx = 0
  and reference_react_label = "r1"
  in
  let lhs2in_map = Bigraph.Iso.of_list [(0, 1); (1, 2); (2, 0)]
  and out2in_map = Bigraph.Fun.of_list [(0, 1); (1, 0); (2, 2); (3, 3); (4, 4); (5, 5)] 
  and expected_out2in_map = [(0,0);(1,2);(2,3);(3,1);(4,4);(5,5)] |> Bigraph.Fun.of_list
  in
  let map_of_states = Hashtbl.create 10 
  and transition_to_norm = 
    { 
      Tracking_bigraph.TTS.in_state_idx=reference_in_state_idx;
      out_state_idx=reference_out_state_idx;
      react_label=reference_react_label;
      participants=lhs2in_map;
      residue=out2in_map;
      actual_out_state=(Bigraph.Big.of_string b0_iso)
    }
  in
  let _ = Hashtbl.add map_of_states 0 (Bigraph.Big.of_string b0_template) ; Hashtbl.add map_of_states 1 (Bigraph.Big.of_string b1_template) in
  let normalized_transition = Norm.normalize_single_transition map_of_states transition_to_norm in
  assert_equal
    ~msg:"Input state idx in normalized transition is not equal to expected"
    reference_in_state_idx
    normalized_transition.in_state_idx ;
  assert_equal 
    ~msg:"Output state idx in normalized transition is not equal to expected"
    reference_out_state_idx
    normalized_transition.out_state_idx;
  assert_equal 
    ~msg:"Reaction label in normalized transition is not equal to expected"
    reference_react_label
    normalized_transition.react_label;
  assert_equal 
    ~msg:"Participants in normalized transition is not equal to expected"
    ~cmp:Bigraph.Iso.equal
    ~printer:Bigraph.Iso.to_string
    lhs2in_map
    normalized_transition.participants;
  assert_equal
    ~msg:"Residue of normalized transition is not equal to expected"
    ~printer:Bigraph.Fun.to_string
    expected_out2in_map
    normalized_transition.residue;
  assert_equal
    ~msg:"Residue test 1 for normalized transition returned value other than expected"
    ~printer:(fun r -> match r with | None -> "None" | Some r' -> string_of_int r')
    (Some 3) 
    (Bigraph.Fun.apply normalized_transition.residue 2);
  assert_equal 
    ~msg:"Residue test 2 for normalized transition returned value other than expected"
    ~printer:(fun r -> match r with | None -> "None" | Some r' -> string_of_int r')
    (Some 2)
    (Bigraph.Fun.apply normalized_transition.residue 1)
let suite =
  "Normalization" >::: [
    "Single transition normalization test 1">:: test_norm_single_transition_1
  ]

let () =
  run_test_tt_main suite
