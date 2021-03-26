let string_list_2_2tuple sl = 
  assert (List.length sl = 2);
  List.nth sl 0,List.nth sl 1
let los2_s los = 
  if (List.length los) <> 1 then
      raise (invalid_arg "invalid row")
  else 
      List.nth los 0
let los2_2tos los = 
  if (List.length los) <> 2 then
      raise (invalid_arg "invalid row")
  else 
      (List.nth los 0,List.nth los 1)
let los2_6tos los = 
  if (List.length los) <> 6 then
      raise (invalid_arg "invalid row")
  else 
      (List.nth los 0,List.nth los 1,List.nth los 2,List.nth los 3,List.nth los 4,List.nth los 5)
let pair_of_ints_from_str_pair str =
  let regex = Str.regexp "[0-9]+" in
  let _ = Str.search_forward regex str 0 in
  let value1 = Str.matched_string str
  and _ = Str.search_forward regex str (Str.match_end ()) in
  let value2 = Str.matched_string str in
      (int_of_string value1,int_of_string  value2)
  let iso_of_string str = 
  let regex = Str.regexp "\\([0-9]+, [0-9]+\\)" in
  let start_searching_idx = ref 0
  and result = ref [] in
  try 
      while true  do 
      let _ =  Str.search_forward regex str !start_searching_idx in
          start_searching_idx := Str.match_end () ;
          result := (pair_of_ints_from_str_pair (Str.matched_string str)) :: !result
      done;
      Bigraph.Iso.of_list !result
  with
  | Not_found -> Bigraph.Iso.of_list !result;;
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
let parse_patterns sll = 
  List.map 
  (
    fun input -> 
      let big_as_string = los2_s input in
      Bigraph.Big.of_string big_as_string
  )
  sll
let parse_react_times file_name =
  let react_times_csv = Csv.load file_name in
  let react_times_list = 
    List.map 
      (
        fun sl -> let name,time_str = string_list_2_2tuple sl in 
          name,int_of_string time_str
      )
      react_times_csv
  and result = Hashtbl.create (List.length react_times_csv) in
  List.iter (fun (name,time) -> Hashtbl.add result name time) react_times_list ; result
let parse_ctrls file_name = 
  let ctrls_csv = Csv.load file_name in
  let ctrls_list = 
    List.map 
      (
        fun sl -> List.hd sl |> Bigraph.Ctrl.of_string
      )
      ctrls_csv in
      ctrls_list
let parse_destingation_patterns file_name =
  let patterns_csv = Csv.load file_name in
  let patterns_list = 
    List.map
      (fun sl -> 
        let big_str,desc = string_list_2_2tuple sl in
        {Patterns.bigraph=(Bigraph.Big.of_string big_str);description=desc}
      )
      patterns_csv in
    patterns_list

