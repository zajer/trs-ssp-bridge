open Common
module IntMap = Map.Make(Int);;
let parse_s sll = 
  List.map 
  (fun input -> 
    let idx,bigraph = los2_2tos input in 
      int_of_string idx, Bigraph.Big.of_string bigraph ) 
  (List.tl sll) 
let parse_t tll = 
  List.map 
  (fun input -> 
      let init_state_idx, res_state_idx, react_label, iso_init2lhs, iso_res2init, res_bigraph = los2_6tos input 
          in int_of_string init_state_idx, int_of_string res_state_idx, react_label, iso_of_string iso_init2lhs, iso_of_string iso_res2init, Bigraph.Big.of_string res_bigraph
  )
  tll;;
let parse_t_without_header tll =
          parse_t (List.tl tll)
