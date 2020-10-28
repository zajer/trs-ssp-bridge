open Common
module IntMap = Map.Make(Int);;
module StringMap = Map.Make(String)
(* Operations needed for generating an .ml file containing definitions of functions based on a transitions saved in a csv file. *)

let extract_ids_of_agents = fun (b:Bigraph.Big.t) ctrls ->  
  List.fold_left (fun result_int_set ctrl -> Bigraph.Nodes.find_all ctrl b.n |> Bigraph.IntSet.union result_int_set) Bigraph.IntSet.empty ctrls
let get_ordering_isomorhpism is = 
  Bigraph.IntSet.fix is
let filter_non_agent_ids_from_iso iso aids = 
  List.filter (fun (i,_) -> Bigraph.IntSet.mem i aids) (Bigraph.Iso.to_list iso) |> Bigraph.Iso.of_list;;
let map_states_with_agent_ids_and_ordering_iso s agent_ctrls = 
  List.fold_left 
    (
      fun map (big_id,big) -> 
        let node_ids_of_agents = extract_ids_of_agents big agent_ctrls in
        let ordering_iso = get_ordering_isomorhpism node_ids_of_agents in
        IntMap.add big_id (node_ids_of_agents,ordering_iso) map
    ) 
    IntMap.empty s
let get_order_of_agents_after_transition mapped_states init_state_id res_state_id residue =
  let agents_in_res_state,_ = try IntMap.find res_state_id mapped_states with Not_found -> raise (Invalid_argument "Not Found in order of agents, fist call")
  and _ ,ordering_iso = try IntMap.find init_state_id mapped_states with Not_found -> raise (Invalid_argument "Not Found in order of agents, second call") in
  let residue_of_agents = filter_non_agent_ids_from_iso residue agents_in_res_state in
  let residue_of_agents_with_transformed_codom_= Bigraph.Iso.transform ~iso_dom:(make_id_iso (Bigraph.Iso.dom residue_of_agents)) ~iso_codom:ordering_iso residue_of_agents
  in
    let _ ,result = Bigraph.Iso.to_list residue_of_agents_with_transformed_codom_ |> List.split in
    result;;
let get_ids_of_agents_participating_in_transition mapped_states init_state_id lhs2init =
  let _ ,ordering_iso = try IntMap.find init_state_id mapped_states with Not_found -> raise (Invalid_argument "Not Found in ids of participating agents") in
  let lhs2init_with_transformed_codom = Bigraph.Iso.transform ~iso_dom:(make_id_iso (Bigraph.Iso.dom lhs2init)) ~iso_codom:ordering_iso lhs2init in
    Bigraph.Iso.codom lhs2init_with_transformed_codom ;;
let construct_trans_function_for_one_agent name label transition_data =
  let num_of_agents = List.length transition_data in
  "let "^name^" e t =\n\tmatch e with\n\t| Unreachable -> Unreachable\n\t| Reachable ("^i_tuple num_of_agents ^",set) ->\n\t\tlet new_state = "^i_tuple ~mody:transition_data num_of_agents^"\n\t\tand new_set = SetOfActions.add {label=\""^label^"\"; step=t+1} set in\n\t\t\tReachable (new_state, new_set)"
let array_of_times_to_check_4_equality synchro_agents = 
  let elems = List.map (fun id -> Common.agent_time_identifier_in_tuple id ) synchro_agents |> String.concat ";" in
  "["^elems^"]"
let construct_trans_function_for_multi_agents name label transition_data agents_in_trans = 
  let num_of_agents = List.length transition_data
  and synchro_agents_array = array_of_times_to_check_4_equality agents_in_trans in
  "let "^name^" e t =\n\tmatch e with\n\t| Unreachable -> Unreachable\n\t| Reachable ("^i_tuple num_of_agents ^",set) ->\n\t\tlet synchro_agents = "^synchro_agents_array^" in\n\t\tlet condition,_ =  List.fold_left (fun (r,v0) v -> r && (v0 = v),v0 ) (true, List.hd synchro_agents) synchro_agents in \n\t\tif condition then\n\t\t\tlet new_state = "^i_tuple ~mody:transition_data num_of_agents^"\n\t\t\tand new_set = SetOfActions.add {label=\""^label^"\"; step=t+1} set in\n\t\t\tReachable (new_state, new_set)\n\t\telse\n\t\t\tUnreachable"
let construct_trans_function name label transition_data agents_in_trans = 
  if List.length agents_in_trans = 1 then
    construct_trans_function_for_one_agent name label transition_data 
  else
    construct_trans_function_for_multi_agents name label transition_data agents_in_trans
let make_transistion_data order_of_agents_after_trans ids_of_agents_participating time_shift =
  List.map (fun ai -> if List.mem ai ids_of_agents_participating then (ai,time_shift) else (ai,0) ) order_of_agents_after_trans
let construct_trans_function mapped_states ~from_idx ~to_idx ~lhs2from ~residue ~name ~label time_shift = 
  let order_of_agents_after_trans = get_order_of_agents_after_transition mapped_states from_idx to_idx residue
  and agents_participating_in_trans = get_ids_of_agents_participating_in_transition mapped_states from_idx lhs2from in
  let transition_data = make_transistion_data order_of_agents_after_trans agents_participating_in_trans time_shift in
    construct_trans_function name label transition_data agents_participating_in_trans
let construct_null_trans_function () = 
  "let f_null _ _ = Unreachable"
let gen_trans_functions s_parsed t_parsed map_trans2time_shift list_of_agents_ctrls= 
  let mapped_s = map_states_with_agent_ids_and_ordering_iso s_parsed list_of_agents_ctrls in
  let functions = List.mapi (fun i (from_idx,to_idx,label,lhs2from,residue,_) -> construct_trans_function mapped_s ~from_idx ~to_idx~lhs2from ~residue ~name:("f"^string_of_int i) ~label:(string_of_int i) (try StringMap.find label map_trans2time_shift with Not_found -> raise (Invalid_argument "Not Found in get trans function")) ) t_parsed
    in
    (construct_null_trans_function ()) :: functions
let gen_trans_functions_based_on_csv ~states_file:(sfn) ~norm_trans_file:(tfn) map_trans2time_shift list_of_agents_ctrls= 
  let s_parsed = Csv.load sfn |> Parsing.parse_s 
  and t_parsed = Csv.load tfn |> Parsing.parse_t in
    gen_trans_functions s_parsed t_parsed map_trans2time_shift list_of_agents_ctrls
let trans_matrix_elem list_of_trans_fun_ids = 
  let elems = if List.length list_of_trans_fun_ids = 0 then "f_null" else List.map (fun i -> "f"^string_of_int i) list_of_trans_fun_ids |> String.concat ";" in
    "["^elems^"]"
let trans_matrix_row list_of_elems = 
  let elems = List.map (fun l -> trans_matrix_elem l ) list_of_elems |> String.concat ";" in
    "[|"^elems^"|]"
let trans_matrix list_of_rows = 
  let elems = List.map (fun l -> trans_matrix_row l ) list_of_rows |> String.concat ";\n\t" in
    "[|\n\t"^elems^"\n|]"
let gen_trans_matrix s_parsed t_parsed = 
  let to_map_update = 
    fun new_elem curr_loi -> match curr_loi with 
    | None -> Some [new_elem]
    | Some loi -> Some (new_elem :: loi) in
  let from_map_update = 
    fun (to_id,new_elem) curr_map_of_loi -> 
      match curr_map_of_loi with 
      | None -> Some (IntMap.singleton to_id [new_elem])
      | Some m -> Some (IntMap.update to_id (to_map_update new_elem) m)
  in
  let num_of_states = List.length s_parsed 
  and map_of_results,_ = List.fold_left 
    (
      fun (res_m,i) (from_id,to_id,_,_,_,_) -> 
        IntMap.update from_id (from_map_update (to_id,i)) res_m,i+1 
    )
    (IntMap.empty,0)
    t_parsed in
    List.init num_of_states 
      (
        fun from_id -> 
          let mx_row_map_op = IntMap.find_opt from_id map_of_results in
          match mx_row_map_op with
          | None -> List.init num_of_states (fun _ -> []) 
          | Some mx_row_map -> 
            List.init num_of_states 
              (
                fun to_id -> 
                  let mx_elem = IntMap.find_opt to_id mx_row_map in
                  match mx_elem with 
                  | None -> []
                  | Some loi -> loi
              )
      ) |> trans_matrix
let gen_trans_matrix_csv ~states_file:(sfn) ~norm_trans_file:(tfn) = 
  let s_parsed = Csv.load sfn |> Parsing.parse_s 
  and t_parsed = Csv.load tfn |> Parsing.parse_t in 
    gen_trans_matrix s_parsed t_parsed
  