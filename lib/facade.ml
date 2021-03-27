open Printf
open Tracking_bigraph
let write_file filename content = 
  let oc = open_out filename in
  fprintf oc "%s\n" content;
  close_out oc;;
let _read_file filename = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; !lines
  with End_of_file ->
    close_in chan ;
    List.rev !lines ;;
let extract_destination_states ~states_file ~patterns_file ~output_file =
  let states = Tracking_bigraph.TTS.import_states states_file
  and patterns = Parsing.parse_destingation_patterns patterns_file in
  let dest_states = Patterns.find_dest_states states patterns in
  Patterns.export_dest_state dest_states output_file
let normalize_tts ~states_file ~trans_file ~norm_trans_file =
  let states = Tracking_bigraph.TTS.import_states states_file
  and transitions = Tracking_bigraph.TTS.import_transitions trans_file in
  let normalized_transitions = Norm.normalize_exported_tts states transitions in
  let normalized_transitions_csv = 
    List.map 
      (
        fun t -> [string_of_int t.TTS.in_state_idx;string_of_int t.out_state_idx;t.react_label;t.participants |> Bigraph.Iso.to_string;t.residue |> Bigraph.Fun.to_string; t.actual_out_state |> Bigraph.Big.to_string]
      )
      normalized_transitions
  in
  Csv.save norm_trans_file normalized_transitions_csv
let transform_tts ~states_file ~norm_trans_file ~react_times_file ~ctrls_file ~ss_file =
  let states = Tracking_bigraph.TTS.import_states states_file
  and transitions = Tracking_bigraph.TTS.import_transitions norm_trans_file
  and react_times = Parsing.parse_react_times react_times_file
  and ctrls = Parsing.parse_ctrls ctrls_file 
  in 
    let transformed_transitions = Trans_fun.convert_transitions states transitions react_times ctrls in
    Ssp.Frontend.export_trans_funs transformed_transitions ss_file
let gen_ssp_source ~states_file ~template_file ~source_file number_of_agents = 
    let states = Tracking_bigraph.TTS.import_states states_file
    and template = _read_file template_file |> String.concat "\n" in
    let source = Mod_gen.construct_module_content_based_on_template template ~number_of_agents ~number_of_states:(List.length states) in
    write_file source_file source
let all_in_one 
  ~in_states_file 
  ~in_patterns_file 
  ~in_trans_file 
  ~in_react_times_file 
  ~in_ctrls_file 
  ~number_of_agents
  ~in_template_file 
  ~out_dest_states_file
  ~out_ssp_input_file 
  ~out_ssp_source_code_file = 
  let states = Tracking_bigraph.TTS.import_states in_states_file
  and patterns = Parsing.parse_destingation_patterns in_patterns_file
  and transitions = Tracking_bigraph.TTS.import_transitions in_trans_file 
  and react_times = Parsing.parse_react_times in_react_times_file
  and ctrls = Parsing.parse_ctrls in_ctrls_file 
  and template = _read_file in_template_file |> String.concat "\n" in
  let dest_states = Patterns.find_dest_states states patterns 
  and normalized_transitions = Norm.normalize_exported_tts states transitions 
  and source = Mod_gen.construct_module_content_based_on_template template ~number_of_agents ~number_of_states:(List.length states) in
  let transformed_transitions = Trans_fun.convert_transitions states normalized_transitions react_times ctrls in
  Ssp.Frontend.export_trans_funs transformed_transitions out_ssp_input_file ;
  Patterns.export_dest_state dest_states out_dest_states_file ;
  write_file out_ssp_source_code_file source
    

