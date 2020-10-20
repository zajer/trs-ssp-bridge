open Trs_bridge

let f1 = Fun_gen.construct_trans_function_for_one_agent "f1" "m1" [(0,0);(1,0)]
let _ = print_endline f1 
let f2 = Fun_gen.construct_trans_function_for_one_agent "f2" "m2" [(0,1);(1,0)]
let _ = print_endline f2 
let f3 = Fun_gen.construct_trans_function_for_one_agent "f3" "m3" [(1,0);(0,1)]
let _ = print_endline f3
let f4 = Fun_gen.construct_trans_function_for_multi_agents "f4" "m4" [(0,1);(1,1)] [0;1]
let _ = print_endline f4