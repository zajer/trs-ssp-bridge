open Common
let rec i_type ?(accu="") number_of_agents =
  match number_of_agents with
  | 0 -> String.sub accu 1 (String.length accu -1)
  | x -> i_type (x-1) ~accu:(accu^"*(int*int)") ;;

let i_to_string_pp_array ?(mody=[]) num_of_agents =
  let data = if List.length mody = 0 then List.init num_of_agents (fun i -> i,0) else mody in
  let common_elem = fun agent_id shift -> "string_of_int "^agent_identifier_in_tuple agent_id ^ " ^ \":\" ^ string_of_int "^ agent_time_identifier_in_tuple ~shift agent_id in
  let list_of_elems = List.init num_of_agents (fun i -> let id,time = List.nth data i in common_elem id time) |> String.concat " ^ \",\" ^ " in
  "\"[\" ^ "^list_of_elems^" ^ \"]\""
  
let construct_set_of_actions_2_string () = 
  "let set_of_actions_2_string set =\n\tlet part1 = SetOfActions.fold (fun el sum -> (el.label^\"^\"^ string_of_int el.step ^\"->\")^sum ) set \"\"\n\tand part2 = \"END\" in\n\t\"{\"^part1 ^ part2^\"}\""
let construct_i_to_string num_of_agents = 
  "let i_to_string (s:i) =\n\tmatch s with\n\t|"^ i_tuple num_of_agents ^ " ->\n\t"^ i_to_string_pp_array num_of_agents ;;

let reachable_elem num_of_agents = 
  "("^i_tuple num_of_agents^",set)"
let construct_k_to_string num_of_agents = 
  "let k_to_string ks =\n\tList.fold_left\n\t\t(fun accu c ->\n\t\t\tmatch c with\n\t\t\t| Unreachable -> accu ^ \"\"\n\t\t\t| Reachable "^reachable_elem num_of_agents^" -> accu ^ ( i_to_string "^i_tuple num_of_agents^" ^ set_of_actions_2_string set )\n\t\t)\n\t\t\"\"\n\t\tks";;

let construct_module module_name number_of_agents =
  let module_header = fun name -> "module "^ String.capitalize_ascii name^" =\n\tstruct" 
  and module_types = fun num_a -> "\ttype i = "^i_type num_a^"\n\ttype t = Unreachable | Reachable of i*SetOfActions.t\n\ttype k = t list\n\ttype f = t -> int -> t" 
  and module_footer = fun () -> "end" in
  module_header module_name ^"\n\n"^ module_types number_of_agents ^ "\n\n"^ construct_set_of_actions_2_string () ^ "\n\n"^  construct_i_to_string number_of_agents ^"\n" ^ construct_k_to_string number_of_agents ^"\n"^ module_footer ();;

let _replace_num_of_agents_in_template num_of_ags template = 
  let regex = Str.regexp "###NUM_OF_AGS###" in
  Str.replace_first regex (string_of_int num_of_ags) template
let _replace_init_state_in_template init_state_str template = 
  let regex = Str.regexp "###INIT_STATE###" in
  Str.replace_first regex init_state_str template
let _replace_num_of_states num_of_sts template =
  let regex = Str.regexp "###NUM_OF_STS###" in
  Str.replace_first regex (string_of_int num_of_sts) template
let _generate_init_state_str num_of_ags = 
  let part_result = List.init num_of_ags (fun i -> "("^(string_of_int (i+1))^",0)") |> String.concat ";" in
  "[|"^part_result^"|]"
let construct_module_content_based_on_template template ~number_of_agents ~number_of_states =
  let init_state_str = _generate_init_state_str number_of_agents in
  _replace_init_state_in_template init_state_str template
  |>
  _replace_num_of_agents_in_template number_of_agents 
  |>
  _replace_num_of_states number_of_states
