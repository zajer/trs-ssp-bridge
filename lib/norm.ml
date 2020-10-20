open Common
module IntMap = Map.Make(Int);;
(* Operations needed for normalizing a csv produced by the TRS library. *)
let normalize_t t_parsed mapped_states = 
  List.map 
  (
      fun (init_state_idx,res_state_idx,react_label,iso_init2lhs,iso_res2init,res_state) -> 
      let new_res_state = IntMap.find res_state_idx mapped_states in
      let avail_isos = Bigraph.Big.occurrences ~target:res_state ~pattern:new_res_state in
      if List.length avail_isos = 0 then
          raise (invalid_arg "incorrect mapping of a result state to an indexed state")
      else
          let iso_to_be_applied,_,_ = List.hd avail_isos in
          let new_iso_res2init = Bigraph.Iso.transform ~iso_dom:iso_to_be_applied ~iso_codom:(Bigraph.Iso.codom iso_res2init |> make_id_iso) iso_res2init in
      (init_state_idx,res_state_idx,react_label,iso_init2lhs,new_iso_res2init,new_res_state)
  )
  t_parsed;;
let t_normalized_2_sll t_norm = 
  List.map 
  (
      fun (init_state_idx,res_state_idx,react_label,iso_init2lhs,new_iso_res2init,new_res_state) -> 
      [string_of_int init_state_idx;string_of_int res_state_idx;react_label; Bigraph.Iso.to_string iso_init2lhs; Bigraph.Iso.to_string new_iso_res2init; Bigraph.Big.to_string new_res_state ]
  )
  t_norm;;
let filename_without_extension fn =
  let regex_str = ".+.csv" in
  let regex = Str.regexp regex_str in
  try 
      let idx_of_extension = Str.search_forward regex fn 0 in
      Str.string_before fn idx_of_extension
  with
  | Not_found -> fn
let make_map_out_of_s s_parsed =
  List.fold_left (fun map (idx,bigraph) -> IntMap.add idx bigraph map) IntMap.empty s_parsed;;
let normalize_exported_ss_no_saving s_parsed t_parsed  =
  let s_mapped = make_map_out_of_s s_parsed in
  let t_normalized = normalize_t t_parsed s_mapped in 
    t_normalized
let normalize_exported_ss s_parsed t_parsed orig_trans_file_name =
  let t_normalized = normalize_exported_ss_no_saving s_parsed t_parsed in
  let t_normalized_sll = t_normalized_2_sll t_normalized in
    Csv.save ((filename_without_extension orig_trans_file_name )^"_normalized.csv") (t_normalized_sll) ;
    t_normalized

