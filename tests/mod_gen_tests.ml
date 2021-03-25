open OUnit2 
open Trs_bridge
let test_construct_program_source_1 _ = 
  let template = "###NUM_OF_AGS###\n###INIT_STATE###\n###NUM_OF_STS###"
  and number_of_agents = 3
  and number_of_states = 777
  and expected_result = "3\n[|(1,0);(2,0);(3,0)|]\n777" in
  let result = Mod_gen.construct_module_content_based_on_template template ~number_of_agents ~number_of_states in
  assert_equal
      ~msg:"Result source is not equal to expected"
      ~printer:(fun s -> "\n"^s^"\n")
      expected_result
      result
let _read_file filename = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; !lines
  with End_of_file ->
    close_in chan ;
    List.rev !lines ;;
let test_construct_program_source_2 _ = 
  let template = _read_file "template.txt" |> String.concat "\n"
  and number_of_agents = 3
  and number_of_states = 777
  and expected_result = _read_file "template_modified.txt" |> String.concat "\n" in
  let result = Mod_gen.construct_module_content_based_on_template template ~number_of_agents ~number_of_states in
  assert_equal
      ~msg:"Result source is not equal to expected"
      ~printer:(fun s -> "\n"^s^"\n")
      expected_result
      result
let suite =
  "Normalization" >::: [
    "Construct program's source tests 1">:: test_construct_program_source_1;
    "Construct program's source tests 2">:: test_construct_program_source_2
    ]

let () =
  run_test_tt_main suite
