let string_list_2_2tuple sl = 
  assert (List.length sl = 2);
  List.nth sl 0,List.nth sl 1
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

