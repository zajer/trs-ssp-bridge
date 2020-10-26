open Trs_bridge
let s_file = "result_states.csv"
let t_file = "result_trans.csv" 
let t2ts_file = "t2ts.csv"
let ctrls_file = "ctrls.csv"
let patts_file = "patterns.csv"

let _ = Facade.full_transformation_saving_norm ~states_file:s_file ~trans_file:t_file ~trans2time_shift_file:t2ts_file ~ctrls_file:ctrls_file ~module_name:"Yolo" ~output_filename:"Swag.ml" 2
let _ = Facade.extract_destination_states ~states_file:s_file ~patterns_file:patts_file ~output_file:"dst_states"