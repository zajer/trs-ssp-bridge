module IntMap = Map.Make(Int)
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
let make_id_iso set = List.map (fun x -> x,x ) set |> Bigraph.Iso.of_list
let string_list_2_2tuple sl = 
  assert (List.length sl = 2);
  List.nth sl 0,List.nth sl 1