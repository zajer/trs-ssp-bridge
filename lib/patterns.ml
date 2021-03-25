type pattern = {bigraph:Bigraph.Big.t;description:string}
type dest_state = Ssp.State.dest_state
let find_destination_states parsed_states patterns = 
  let pattern_detection_fun = fun ptrns target -> List.exists (fun p -> Bigraph.Big.occurs ~target ~pattern:p ) ptrns in
  List.map 
    (
      fun (big_id,big) -> 
        
        let if_occs = pattern_detection_fun patterns big in
          big_id,if_occs
    )
    parsed_states
    |> List.filter_map (fun (id,flag) -> if flag then Some id else None )
(*let find_final_states_csv ~states_file ~patterns_file = 
  let states_parsed = Csv.load states_file |> Parsing.parse_s
  and patterns = Csv.load patterns_file |> Parsing.parse_patterns in
  find_destination_states states_parsed patterns*)
let find_dest_states states patterns =
  let pattern_detection_fun = 
    fun target ptrns -> 
      List.filter_map 
        (
          fun p -> 
            if Bigraph.Big.occurs ~target ~pattern:p.bigraph then
              Some (p.description)
            else
              None
        )
        ptrns
    in
  Parmap.parmap 
  (
    fun s -> 
      let patterns_detected = pattern_detection_fun s.Tracking_bigraph.TTS.bigraph patterns in
      {Ssp.State.state_idx=s.index;patts_found=patterns_detected}
  )
  (Parmap.L states)
  |> List.filter (fun ds -> match ds.Ssp.State.patts_found with | [] -> false | _::_ -> true)
let _string_list_2_string sl = 
  "["^(String.concat ";" sl)^"]"
let export_dest_state dest_states file_name =
  let dest_states_csv = List.map 
    (
      fun ds -> [string_of_int ds.Ssp.State.state_idx;_string_list_2_string ds.patts_found]
    )
    dest_states
  in
  Csv.save file_name dest_states_csv
let rec _find_inner_elts str start_position accu = 
  let inner_elt_regex = Str.regexp "[a-zA-Z0-9 ]+" in
  try
    let _ = Str.search_forward inner_elt_regex str start_position
    and matched = Str.matched_string str
    and new_start_pos = Str.match_end () in
      _find_inner_elts str new_start_pos (matched::accu)
  with Not_found -> accu
  (*
let _find_last_elt str start_position = 
  let last_elt_regex = Str.regexp "[a-zA-Z0-9 ]+" in
  try
    let _ = Str.search_forward last_elt_regex str start_position
    and matched = Str.matched_string str in
      Some matched
  with Not_found -> None*)
let _parse_list_of_string str = 
  let inner_elts = _find_inner_elts str 0 [] in
  (*let last_elt_opt = _find_last_elt str new_start in
  match last_elt_opt with
  | Some last_elt -> last_elt::inner_elts
  | None -> inner_elts*)
  inner_elts
let import_dest_states file_name =
  let dest_states_csv = Csv.load file_name in
  let dest_states =
    List.map 
      (
        fun sl -> let idx_str,string_list_str = Common.string_list_2_2tuple sl in
        {Ssp.State.state_idx=(int_of_string idx_str); patts_found= _parse_list_of_string string_list_str}
      )
      dest_states_csv
  in
    dest_states