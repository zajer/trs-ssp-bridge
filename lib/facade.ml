open Printf
module StringMap = Map.Make(String)
open Tracking_bigraph
let parse_trans2timeshift_map sll =
  let raw_data =List.map 
    (
      fun row ->
        let trans_name,time_shift = Common.los2_2tos row in
        trans_name,int_of_string time_shift
    )
    sll in
  List.fold_left (fun res_map (trans_name,time_shift) -> StringMap.add trans_name time_shift res_map) StringMap.empty raw_data
let parse_agents_ctrls sll =
  List.map (fun input -> Common.los2_s input) sll |> List.map (fun str -> Bigraph.Ctrl.of_string str)
let write_file filename content = 
  let oc = open_out filename in
  fprintf oc "%s\n" content;
  close_out oc;;
let full_transformation_saving_norm ~states_file:sfn ~trans_file:tfn ~trans2time_shift_file:t2tsfn ~ctrls_file:cfn ~module_name ~output_filename ?(separate_functions = false) number_of_agents =
  let s = Csv.load sfn 
  and t = Csv.load tfn in
  let s_parsed = Parsing.parse_s s
  and t_parsed = Parsing.parse_t_without_header t 
  and trans2timeshift = Csv.load t2tsfn |> parse_trans2timeshift_map 
  and agents_ctrls = Csv.load cfn |> parse_agents_ctrls in
  let norm = Norm.normalize_exported_ss s_parsed t_parsed tfn in
  let module_file_content = Mod_gen.construct_module module_name number_of_agents
  and functions = Fun_gen.gen_trans_functions s_parsed norm trans2timeshift agents_ctrls
  and fun_matrix = Fun_gen.gen_trans_matrix s_parsed norm in
  let main_file_content = module_file_content :: (if not separate_functions then functions @ [fun_matrix] else [fun_matrix]) |> String.concat "\n" 
  and additional_file_content = functions |> String.concat "\n" in
    write_file output_filename main_file_content;
    if separate_functions then write_file ((Common.filename_without_extension "ml" output_filename)^"_functions.ml") additional_file_content else () ;;
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
    Trans_fun.export_trans_funs transformed_transitions ss_file


