open Common
open Bigraph
open Tracking_bigraph
module IntMap = Map.Make(Int);;
type states = (int, Big.t) Hashtbl.t

let map_states sl =
  let result = Hashtbl.create ~random:false (List.length sl) in
  List.iteri (fun i s -> Hashtbl.add result i s.TTS.bigraph) sl; result

(* Operations needed for normalizing a csv produced by the TRS library. *)
let normalize_transition mapped_states (init_state_idx,res_state_idx,react_label,iso_init2lhs,iso_res2init,res_state)  =
  let new_res_state = IntMap.find res_state_idx mapped_states in
  let avail_isos = Bigraph.Big.occurrences ~target:res_state ~pattern:new_res_state in
  if List.length avail_isos = 0 then
      raise (invalid_arg "incorrect mapping of a result state to an indexed state")
  else
      let iso_to_be_applied,_,_ = List.hd avail_isos in
      let new_iso_res2init = Bigraph.Iso.transform ~iso_dom:( Bigraph.Iso.inverse iso_to_be_applied) ~iso_codom:(Bigraph.Iso.codom iso_res2init |> make_id_iso) iso_res2init in
  (init_state_idx,res_state_idx,react_label,iso_init2lhs,new_iso_res2init,new_res_state)
let normalize_single_transition mapped_states trans = 
  let new_out_state = Hashtbl.find mapped_states trans.TTS.out_state_idx in
  let iso_to_be_applied_on_actual_out_state = TBig.translate_equal ~from_b:new_out_state ~to_b:trans.actual_out_state  in
    let new_iso_res2init = Fun.transform 
      ~iso_dom:iso_to_be_applied_on_actual_out_state 
      ~iso_codom:(Fun.to_list trans.residue |> List.map (fun (_,i2) -> i2 ) |> make_id_iso) 
      trans.residue in
  {
    TTS.in_state_idx=trans.in_state_idx;
    out_state_idx=trans.out_state_idx;
    react_label=trans.react_label;
    participants=trans.participants;
    residue=new_iso_res2init;
    actual_out_state=new_out_state
  }
let normalize_transitions mapped_states tl = 
  List.map
    (normalize_single_transition mapped_states)
    tl

let normalize_t t_parsed mapped_states = 
  List.map 
  (normalize_transition mapped_states)
  t_parsed;;
let t_normalized_2_sll t_norm = 
  List.map 
  (
      fun (init_state_idx,res_state_idx,react_label,iso_init2lhs,new_iso_res2init,new_res_state) -> 
      [string_of_int init_state_idx;string_of_int res_state_idx;react_label; Bigraph.Iso.to_string iso_init2lhs; Bigraph.Iso.to_string new_iso_res2init; Bigraph.Big.to_string new_res_state ]
  )
  t_norm;;
let make_map_out_of_s s_parsed =
  List.fold_left (fun map (idx,bigraph) -> IntMap.add idx bigraph map) IntMap.empty s_parsed;;
let normalize_exported_ss_no_saving s_parsed t_parsed  =
  let s_mapped = make_map_out_of_s s_parsed in
  let t_normalized = normalize_t t_parsed s_mapped in 
    t_normalized
let normalize_exported_ss s_parsed t_parsed orig_trans_file_name =
  let t_normalized = normalize_exported_ss_no_saving s_parsed t_parsed in
  let t_normalized_sll = t_normalized_2_sll t_normalized in
    Csv.save ((filename_without_extension "csv" orig_trans_file_name )^"_normalized.csv") (t_normalized_sll) ;
    t_normalized
let normalize_exported_tts s_parsed t_parsed =
  let states_mapped = map_states s_parsed in
    normalize_transitions states_mapped t_parsed
let normalize_exported_ss_csv ~states_file:sfn ~trans_file:tfn =
  let s = Csv.load sfn |> Parsing.parse_s
  and t = Csv.load tfn |> Parsing.parse_t_without_header in
  normalize_exported_ss s t tfn