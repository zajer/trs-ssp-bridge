(* Operations needed for normalizing a csv produced by the TRS library. *)
open Bigraph
open Tracking_bigraph
type mapped_states = Big.t option array
let _make_id_iso set = List.map (fun x -> x,x ) set |> Bigraph.Iso.of_list
let map_states sl =
  let result = Array.init (List.length sl) (fun _ -> None) in
  List.iter (fun s -> Array.set result s.TTS.index (Some s.TTS.bigraph)) sl; result
let normalize_single_transition mapped_states trans = 
  let new_out_state_opt = Array.get mapped_states trans.TTS.out_state_idx in
  let new_out_state = match new_out_state_opt with 
    | Some s -> s 
    | None -> raise (Invalid_argument ("There is no state with index:"^(string_of_int trans.out_state_idx) )) in
  let iso_to_be_applied_on_actual_out_state = TBig.translate_equal ~to_b:new_out_state ~from_b:trans.actual_out_state  in
    let new_iso_res2init = Fun.transform 
      ~iso_dom:iso_to_be_applied_on_actual_out_state 
      ~iso_codom:(Fun.to_list trans.residue |> List.map (fun (_,i2) -> i2 ) |> _make_id_iso) 
      trans.residue in
  {
    TTS.in_state_idx=trans.in_state_idx;
    out_state_idx=trans.out_state_idx;
    react_label=trans.react_label;
    participants=trans.participants;
    residue=new_iso_res2init;
    actual_out_state=new_out_state
  }
let _norm_map_func mapped_states = fun i trans ->
      try
        normalize_single_transition mapped_states trans
      with Invalid_argument e -> raise (Invalid_argument ("Error while normalizing transition:"^(string_of_int i)^" supposedly equal to state:"^(string_of_int trans.out_state_idx )^" Inner error:"^e ))
let normalize_transitions mapped_states tl = 
  List.mapi
    (_norm_map_func mapped_states)
    tl
let parnormalize_transitions mapped_states tl =
  Parmap.parmapi
  (_norm_map_func mapped_states)
  (Parmap.L tl)
let normalize_exported_tts s_parsed t_parsed =
  let states_mapped = map_states s_parsed in
    normalize_transitions states_mapped t_parsed
let parnormalize_exported_tts s_parsed t_parsed =
  let states_mapped = map_states s_parsed in
    parnormalize_transitions states_mapped t_parsed