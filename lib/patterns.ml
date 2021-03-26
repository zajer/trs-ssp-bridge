type pattern = {bigraph:Bigraph.Big.t;description:string}
type dest_state = Ssp.State.dest_state
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
let _patts_detected_list_2_string sl = 
  (String.concat "\n" sl)
let export_dest_state dest_states file_name =
  let dest_states_csv = List.map 
    (
      fun ds -> [string_of_int ds.Ssp.State.state_idx;_patts_detected_list_2_string ds.patts_found]
    )
    dest_states
  in
  Csv.save file_name dest_states_csv