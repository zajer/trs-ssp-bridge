open Trs_bridge
let s_file = "result_states.csv"
let t_file = "result_trans.csv" 
let t2ts_file = "t2ts.csv"
let ctrls_file = "ctrls.csv"

let _ = Facade.full_saving_norm ~states_file:s_file ~trans_file:t_file ~trans2time_shift_file:t2ts_file ~ctrls_file:ctrls_file ~module_name:"Yolo" ~output_filename:"Swag.ml" 2