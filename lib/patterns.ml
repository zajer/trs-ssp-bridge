let find_destination_states parsed_states patterns = 
  let pattern_detection_fun = fun ptrns target -> List.exists (fun p -> Bigraph.Big.occurs ~target ~pattern:p ) ptrns in
  List.map 
    (
      fun (big_id,big) -> 
        
        let if_occs = pattern_detection_fun patterns big in
          big_id,if_occs
    )
    parsed_states
    |> List.filter_map (fun (id,flag) -> if flag then Some id else None )
let find_final_states_csv ~states_file ~patterns_file = 
  let states_parsed = Csv.load states_file |> Parsing.parse_s
  and patterns = Csv.load patterns_file |> Parsing.parse_patterns in
  find_destination_states states_parsed patterns