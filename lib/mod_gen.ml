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
