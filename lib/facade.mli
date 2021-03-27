val extract_destination_states : states_file:string -> patterns_file:string -> output_file:string -> unit
val normalize_tts : states_file:string -> trans_file:string -> norm_trans_file:string -> unit
val transform_tts : states_file:string -> norm_trans_file:string -> react_times_file:string -> ctrls_file:string -> ss_file:string -> unit
val gen_ssp_source : states_file:string -> template_file:string -> source_file:string -> int -> unit
val all_in_one :
  in_states_file:string ->
  in_patterns_file:string ->
  in_trans_file:string ->
  in_react_times_file:string ->
  in_ctrls_file:string ->
  number_of_agents:int ->
  in_template_file:string ->
  out_dest_states_file:string ->
  out_ssp_input_file:string ->
  out_ssp_source_code_file:string
  -> unit