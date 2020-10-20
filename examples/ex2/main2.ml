open Trs_bridge

let s_file = "result_states.csv"
let t_file = "result_trans_mod_normalized.csv" 
module StringMap = Map.Make(String)
let trans2timeshift = StringMap.singleton "r1" 1 |> StringMap.add "r2" 3 |> StringMap.add "r3" 1  
let agents_controls = [Bigraph.Ctrl.of_string "A:0"; Bigraph.Ctrl.of_string "B:0"]

let x = Fun_gen.gen_trans_functions_based_on_csv ~states_file:s_file ~norm_trans_file:t_file trans2timeshift agents_controls
let _ = List.iter (fun s -> print_endline s ) x
let mf_x = Fun_gen.gen_trans_matrix_csv ~states_file:s_file ~norm_trans_file:t_file
let _ = print_endline mf_x