open Trs_bridge
let s_file = "result_states.csv"
let t_file = "result_trans_mod.csv" ;;
let x = Norm.normalize_exported_ss_csv ~trans_file:t_file ~states_file:s_file