(* Operations needed for normalizing a csv produced by the TRS library. *)
open Bigraph
open Tracking_bigraph
let make_id_iso set = List.map (fun x -> x,x ) set |> Bigraph.Iso.of_list
let map_states sl =
  let result = Hashtbl.create ~random:false (List.length sl) in
  List.iter (fun s -> Hashtbl.add result s.TTS.index s.TTS.bigraph) sl; result
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
let t_normalized_2_sll t_norm = 
  List.map 
  (
      fun (init_state_idx,res_state_idx,react_label,iso_init2lhs,new_iso_res2init,new_res_state) -> 
      [string_of_int init_state_idx;string_of_int res_state_idx;react_label; Bigraph.Iso.to_string iso_init2lhs; Bigraph.Iso.to_string new_iso_res2init; Bigraph.Big.to_string new_res_state ]
  )
  t_norm;;
let normalize_exported_tts s_parsed t_parsed =
  let states_mapped = map_states s_parsed in
    normalize_transitions states_mapped t_parsed