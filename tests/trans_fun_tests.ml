open OUnit2
module OIntSet = Set.Make(Int)
open Trs_bridge
open Bigraph
let _compare_trans_fun_data l1 l2 = 
  if List.length l1 = List.length l2 then
    List.for_all2 (fun el1 el2 -> el1 = el2) l1 l2
  else
    false
let _compare_trans_funs = (fun tf1 tf2 -> (_compare_trans_fun_data tf1.Ssp.State.permutation_with_time_shift tf2.Ssp.State.permutation_with_time_shift) && tf1.react_label = tf2.react_label && tf1.transition_idx = tf2.transition_idx)
let _converted_state_2_string id loip =
  let id_str = (string_of_int id)
  and loip_str = List.map (fun (nid,aid) -> "("^(string_of_int nid)^","^(string_of_int aid)^")" ) loip |> String.concat ";" in
  "{"^id_str^","^loip_str^"}"
let _hashtbl_2_string ht = 
  Hashtbl.fold (fun k loip res -> _converted_state_2_string k loip ::res ) ht [] |> String.concat "|"
let _trans_fun_data_2_string tfd = 
  let trans_fun_data_str = List.map (fun (aid,tshift) -> "("^(string_of_int aid)^","^(string_of_int tshift)^")" ) tfd |> String.concat ";" in
  "["^trans_fun_data_str^"]"
let _trans_fun_2_string tf = 
  let tfd_str = _trans_fun_data_2_string tf.Ssp.State.permutation_with_time_shift in
  "{data="^tfd_str^";rl="^tf.react_label^"}"
let test_transition_function_data_1 _ =
  let permutation = [1;2;3;4]
  and participants = OIntSet.empty |> OIntSet.add 2 |> OIntSet.add 4 in
  let trans_fun_data_result = Trans_fun.transition_function_data permutation participants 777
  and trans_fun_data_expected = [(1,0);(2,777);(3,0);(4,777)] in
  assert_equal 
    ~msg:"Transition function data is not equal to expected"
    ~cmp:_compare_trans_fun_data
    trans_fun_data_expected trans_fun_data_result
let test_transition_function_data_2 _ =
  let permutation = [4;3;2;1]
  and participants = OIntSet.empty |> OIntSet.add 2 |> OIntSet.add 4 in
  let trans_fun_data_result = Trans_fun.transition_function_data permutation participants 777
  and trans_fun_data_expected = [(4,777);(3,0);(2,777);(1,0)] in
  assert_equal 
    ~msg:"Transition function data is not equal to expected"
    ~cmp:_compare_trans_fun_data
    trans_fun_data_expected trans_fun_data_result
let test_transition_function_data_3 _ =
  let permutation = [4;1;2;3]
  and participants = OIntSet.empty |> OIntSet.add 2 |> OIntSet.add 4 in
  let trans_fun_data_result = Trans_fun.transition_function_data permutation participants 777
  and trans_fun_data_expected = [(4,777);(1,0);(2,777);(3,0)] in
  assert_equal 
    ~msg:"Transition function data is not equal to expected"
    ~cmp:_compare_trans_fun_data
    trans_fun_data_expected trans_fun_data_result
let test_convert_states_1 _ = 
  let b1 = "{(0, A:2),(1, A:2),(2, A:2),(3, A:2),(4, U:0)}\n0 5 0\n00000\n00000\n00001\n00000\n00000\n({}, {}, {(0, 1), (1, 1)})\n({}, {}, {(0, 1), (2, 1)})\n({}, {}, {(1, 1), (3, 1)})\n({}, {}, {(3, 1), (2, 1)})" |> Bigraph.Big.of_string
  and b2 = "{(0, A:0),(1, U:0),(2, C:0),(3, D:0),(4, E:0)}\n0 5 0\n01100\n00010\n00001\n00000\n00000" |> Bigraph.Big.of_string
  and agent_ctrls = [Bigraph.Ctrl.of_string "U:0";Bigraph.Ctrl.of_string "D:0"] 
  and expected_mapping = Hashtbl.create 2 in
  let imported_states = [{Tracking_bigraph.TTS.bigraph=b1;index=1};{bigraph=b2;index=2}] in
  let result_mapping = Trans_fun.convert_states imported_states agent_ctrls
  and _ = Hashtbl.add expected_mapping 1 [(4,1)];Hashtbl.add expected_mapping 2 [(1,1);(3,2)] in
  assert_equal ~msg:"Converted states are not equal to expected" ~printer:_hashtbl_2_string expected_mapping result_mapping
let test_convert_single_trans_1 _ =
  let mapped_states = Hashtbl.create 2
  and react_times = Hashtbl.create 2
  and trans_to_convert,trans_to_convert_id = 
    {
      Tracking_bigraph.TTS.in_state_idx = 777;
      out_state_idx = 21;
      react_label = "my_super_label";
      participants = (Iso.empty |> Iso.add 0 4 |> Iso.add 1 1 );
      residue = (Fun.empty |> Fun.add 0 4 |> Fun.add 1 3 |> Fun.add 2 1 |> Fun.add 1 2 );
      actual_out_state = Big.zero
    },777
  in
  let _ = Hashtbl.add mapped_states 777 [(3,1);(4,2)] ; Hashtbl.add mapped_states 21 [(0,1);(1,2)]
  and _ = Hashtbl.add react_times "yolo_react" 3 ; Hashtbl.add react_times "my_super_label" 7 in
  let expected_trans_fun = {Ssp.State.permutation_with_time_shift=[(2,7);(1,0)];react_label="my_super_label";from_idx=777;to_idx=21;transition_idx=trans_to_convert_id}
  and result_trans_fun = Trans_fun.convert_trans_2_trans_fun trans_to_convert trans_to_convert_id mapped_states react_times in
  assert_equal 
    ~msg:"Converted transition function is not equal to expected" 
    ~cmp: _compare_trans_funs
    ~printer: _trans_fun_2_string
    expected_trans_fun 
    result_trans_fun
let test_convert_single_trans_2 _ =
  let mapped_states = Hashtbl.create 2
  and react_times = Hashtbl.create 2
  and trans_to_convert,trans_to_convert_id = 
    {
      Tracking_bigraph.TTS.in_state_idx = 777;
      out_state_idx = 21;
      react_label = "my_super_label";
      participants = (Iso.empty |> Iso.add 0 4 |> Iso.add 1 1 |> Iso.add 2 3);
      residue = (Fun.empty |> Fun.add 0 4 |> Fun.add 1 3 |> Fun.add 2 1 |> Fun.add 1 2 );
      actual_out_state = Big.zero
    }, 777
  in
  let _ = Hashtbl.add mapped_states 777 [(3,1);(4,2)] ; Hashtbl.add mapped_states 21 [(0,1);(1,2)]
  and _ = Hashtbl.add react_times "yolo_react" 3 ; Hashtbl.add react_times "my_super_label" 7 in
  let expected_trans_fun = {Ssp.State.permutation_with_time_shift=[(2,7);(1,7)];react_label="my_super_label";from_idx=777;to_idx=21;transition_idx=trans_to_convert_id}
  and result_trans_fun = Trans_fun.convert_trans_2_trans_fun trans_to_convert trans_to_convert_id mapped_states react_times in
  assert_equal 
    ~msg:"Converted transition function is not equal to expected" 
    ~cmp: _compare_trans_funs
    ~printer: _trans_fun_2_string
    expected_trans_fun 
    result_trans_fun
let suite =
  "Transition function generation" >::: [
    "Transition function data generation test 1">:: test_transition_function_data_1;
    "Transition function data generation test 2">:: test_transition_function_data_2;
    "Transition function generation test 3">:: test_transition_function_data_3;
    "States conversion 1">:: test_convert_states_1;
    "Transition conversion test 1 " >:: test_convert_single_trans_1;
    "Transition conversion test 2 " >:: test_convert_single_trans_2
]

let () =
  run_test_tt_main suite