open OUnit2
module OIntSet = Set.Make(Int)
open Trs_bridge
let _compare_trans_fun_data l1 l2 = 
  List.for_all2 (fun el1 el2 -> el1 = el2) l1 l2
let _converted_state_2_string id loip =
  let id_str = (string_of_int id)
  and loip_str = List.map (fun (nid,aid) -> "("^(string_of_int nid)^","^(string_of_int aid)^")" ) loip |> String.concat ";" in
  "{"^id_str^","^loip_str^"}"
let _hashtbl_2_string ht = 
  Hashtbl.fold (fun k loip res -> _converted_state_2_string k loip ::res ) ht [] |> String.concat "|"
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
  assert_equal ~printer:_hashtbl_2_string expected_mapping result_mapping

let suite =
  "Transition function generation" >::: [
    "Transition function generation test 1">:: test_transition_function_data_1;
    "Transition function generation test 2">:: test_transition_function_data_2;
    "Transition function generation test 3">:: test_transition_function_data_3;
    "States conversion 1">:: test_convert_states_1
]

let () =
  run_test_tt_main suite