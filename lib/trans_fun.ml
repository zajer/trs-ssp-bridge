module IntMap = Map.Make(Int);;
module StringMap = Map.Make(String);;
module OIntSet = Set.Make(Int);;
(* Each state id is mapped to the list of ints (representing ids of nodes) that correspond to relative agents order in that state *)
type state_id_2_agent_node_ids_with_fix_map = (int, (int*int) list) Hashtbl.t
type mapped_states = state_id_2_agent_node_ids_with_fix_map
type t = {permutation_with_time_shift:(int*int) list; react_label:string}
type paired_trans_fun = t*int
type react_times = (string,int) Hashtbl.t
(*#################NOWE###################################*)
let _bintset_2_int_list bis =
  Bigraph.IntSet.fold (fun i res -> i::res) bis []
(* Bigraph.Nodes.find_all considers only the control type. It ommits arity of a control!*)
let _extract_agent_node_ids bigraph list_of_ctrls =
  List.fold_left 
    (
      fun res ctrl -> 
        let list_of_nodes_ids_with_specific_ctrl = Bigraph.Nodes.find_all ctrl bigraph.Bigraph.Big.n |> _bintset_2_int_list in 
        List.append list_of_nodes_ids_with_specific_ctrl res 
    ) 
    [] 
    list_of_ctrls
let map_agent_node_ids_2_relative_agent_ids list_of_agent_node_ids = 
  let sorted_nodes = List.sort (fun i1 i2 -> i1 - i2 ) list_of_agent_node_ids in
  let sorted_nodes_with_fix = List.mapi (fun i agent_node_id -> agent_node_id,i+1) sorted_nodes in
  sorted_nodes_with_fix
let _filter_ids_of_agents residue mapped_states out_state_idx =
  let sub_residue = 
    List.fold_left 
      (fun res (agent_node_in_out_state,_) -> (agent_node_in_out_state,(Bigraph.Fun.apply residue agent_node_in_out_state |> Option.get)) :: res)
      []
      (Hashtbl.find mapped_states out_state_idx)
  in
    sub_residue
let _fix_numbering_of_dom residue_as_list =
  let residue_sorted_by_dom = List.sort (fun (a1,_) (a2,_) -> a1-a2 ) residue_as_list in
  let residue_with_fixed_dom = List.mapi (fun i (_,node_id_in_input_state) -> i+1,node_id_in_input_state ) residue_sorted_by_dom in
  residue_with_fixed_dom
let _relative_agents_2_node_ids_after_trans residue_with_fixed_dom_as_list =
  let residue_sorted_by_codom = 
    List.sort 
      (fun (_,v1) (_,v2) -> v1-v2)
      residue_with_fixed_dom_as_list in
  let permutation = 
    List.mapi 
      (fun i (agent_node_in_out_state,_) -> i+1,agent_node_in_out_state )
      residue_sorted_by_codom in
  permutation
let permutation_of_relative_agents_ids_after_trans residue_with_fixed_dom_as_list =
  let rel_agents_2_node_ids = _relative_agents_2_node_ids_after_trans residue_with_fixed_dom_as_list in
  let permutation_of_rel_agents = _fix_numbering_of_dom rel_agents_2_node_ids in
  let _, result = List.split permutation_of_rel_agents in
  result
let extract_ids_of_agents_participating_in_trans part_fun list_of_agent_node_ids_in_input_state_with_fix =
  let codom_of_part_fun_filtered_of_nonagent_nodes_and_codom_transformed_to_relative_agent_id =
    List.filter_map
      (
        fun (_,in_state_node_id) -> 
          let mapping_opt = List.find_opt (fun (agent_node_id,_) -> in_state_node_id = agent_node_id) list_of_agent_node_ids_in_input_state_with_fix in
          if Option.is_some mapping_opt then
            let mapping = Option.get mapping_opt in
            match mapping with
            | (_,agent_id) -> Some agent_id
          else
            None
      ) 
      (Bigraph.Iso.to_list part_fun) in
      OIntSet.of_list codom_of_part_fun_filtered_of_nonagent_nodes_and_codom_transformed_to_relative_agent_id 
let transition_function_data permutation_of_agents_ids_after_trans ids_of_agents_participating_in_trans time_shift =
  List.map
    (
      fun agent_id -> 
        if OIntSet.mem agent_id ids_of_agents_participating_in_trans then 
          (agent_id, time_shift) 
        else (agent_id, 0)
    )
    permutation_of_agents_ids_after_trans
let convert_trans_2_trans_fun trans mapped_states time_shift_map = 
  let sub_residue = _filter_ids_of_agents trans.Tracking_bigraph.TTS.residue mapped_states trans.out_state_idx in
  let residue_with_fixed_dom_as_list = _fix_numbering_of_dom sub_residue in
  let permutation = permutation_of_relative_agents_ids_after_trans residue_with_fixed_dom_as_list
  and participants = extract_ids_of_agents_participating_in_trans trans.participants (Hashtbl.find mapped_states trans.in_state_idx) in
  let trans_fun_data = transition_function_data permutation participants (Hashtbl.find time_shift_map trans.react_label) in
  {permutation_with_time_shift=trans_fun_data;react_label=trans.react_label}
let convert_states states_list agent_ctrls_list = 
  let result = Hashtbl.create (List.length states_list)
  and transformed_states = 
    List.map 
      (
        fun state -> 
          let state_idx = state.Tracking_bigraph.TTS.index
          and list_of_agent_nodes = _extract_agent_node_ids state.bigraph agent_ctrls_list in
          let list_of_agent_nodes_mapped_with_relative_agent_ids = map_agent_node_ids_2_relative_agent_ids list_of_agent_nodes in
          state_idx,list_of_agent_nodes_mapped_with_relative_agent_ids
      ) states_list
  in
  List.iter 
    (fun (s_idx,list) -> Hashtbl.add result s_idx list ) 
    transformed_states;
  result
let convert_transitions imported_states imported_trans time_shifts agent_ctrls =
  let mapped_states = convert_states imported_states agent_ctrls in
  let converted_trans = 
    List.mapi 
      (fun i t -> convert_trans_2_trans_fun t mapped_states time_shifts,i )
      imported_trans 
  in
  converted_trans
let parconvert_transitions imported_states imported_trans time_shifts agent_ctrls =
  let mapped_states = convert_states imported_states agent_ctrls in
  let converted_trans = 
    Parmap.parmapi 
      (fun i t -> convert_trans_2_trans_fun t mapped_states time_shifts,i )
      (Parmap.L imported_trans)
  in
  converted_trans
let _TRANS_FUN_DATA_HEADER = "permutation with time shift"
let _TRANS_FUN_REACT_HEADER = "react"
let _TRANS_FUN_CORRESPONDING_TRANSITION = "corresponds to transition"
let _trans_fun_data_2_string tfd =
  List.map (fun (aid,ts) -> "("^(string_of_int aid)^","^(string_of_int ts)^")") tfd |> String.concat ";"
let export_trans_funs paired_tfs filename =
  let tfs_csv = List.map (fun (tf,tid) -> [(_trans_fun_data_2_string tf.permutation_with_time_shift);(tf.react_label);(string_of_int tid)]) paired_tfs
  and header = [_TRANS_FUN_DATA_HEADER;_TRANS_FUN_REACT_HEADER;_TRANS_FUN_CORRESPONDING_TRANSITION] in
  Csv.save filename (header::tfs_csv)
let _extract_next_number pair_of_numbers_str start_position =
  let number_regex = Str.regexp "[0-9]+" in
  let _ = Str.search_forward number_regex pair_of_numbers_str start_position 
  and number =  Str.matched_string pair_of_numbers_str |> int_of_string
  and new_start_pos = Str.match_end () in
  number,new_start_pos
let _extract_next_pair_of_numbers tfd_str start_position =
  let pair_of_numbers_regex = Str.regexp "[0-9]+,[0-9]+" in
  let _ = Str.search_forward pair_of_numbers_regex tfd_str start_position in
  let matched = Str.matched_string tfd_str 
  and result_new_start_pos = Str.match_end () in
  let number1,new_start_pos = _extract_next_number matched 0 in
  let number2,_ = _extract_next_number matched new_start_pos  in
  number1,number2,result_new_start_pos
let _parse_trans_fun_data tfd_str =
  let result = ref [] 
  and parse_pointer = ref 0 
  and end_flag = ref false
  in
    while not !end_flag do
    (
      try 
        let num1,num2,pointer_tmp = _extract_next_pair_of_numbers tfd_str !parse_pointer in
        parse_pointer := pointer_tmp;
        result := (num1,num2)::!result;   
      with Not_found -> end_flag := true
    )
    done;
    !result
let _string_list_2_3tuple sl = 
  assert (List.length sl = 3);
  List.nth sl 0,List.nth sl 1,List.nth sl 2
let import_trans_funs filename = 
  let imported_trans_funs_sll = Csv.load filename |> List.tl in
  List.map 
    (
      fun sl ->
        let tf_data_str,react_label,correspondence_numb_str = _string_list_2_3tuple sl in
          let tf_data = _parse_trans_fun_data tf_data_str |> List.rev
          and correspondence_numb = int_of_string correspondence_numb_str in
          {permutation_with_time_shift=tf_data;react_label},correspondence_numb
    )
    imported_trans_funs_sll