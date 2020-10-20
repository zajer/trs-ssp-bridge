open Printf
module StringMap = Map.Make(String)
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
let full_saving_norm ~states_file:sfn ~trans_file:tfn ~trans2time_shift_file:t2tsfn ~ctrls_file:cfn ~module_name ~output_filename number_of_agents =
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
  let file_content = module_file_content :: functions @ [fun_matrix] |> String.concat "\n" in
    write_file output_filename file_content