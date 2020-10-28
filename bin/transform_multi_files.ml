open Trs_bridge

let help () = 
  print_endline 
  "
  This program is intended to transform an output of Tracking_bigraph library into an easily modifiable template of an OCaml program using Policy library.
  The output of this file are two files. The first file contains a module and transition matrix. The second file contains transition functions definitions.
  Arg1 - states file (generated by Trackign_bigraph library).
  Arg2 - transitions file (generated by Trackign_bigraph library).
  Arg3 - a csv file containing a mapping of transition names to time they require.
  Arg4 - a csv file containing a controls (in bigraphical sense) that may represent agents.
  Arg5 - the name of the generated module.
  Arg6 - the name of the output file.
  Arg7 - number of agents in states.
  "
  let () = 
  if Array.length Sys.argv = 8 then
  let states_file = Sys.argv.(1)
  and trans_file = Sys.argv.(2)
  and trans2timeshift_file = Sys.argv.(3)
  and ctrls_file = Sys.argv.(4)
  and module_name = Sys.argv.(5)
  and output_file = Sys.argv.(6)
  and num_of_agents = Sys.argv.(7) |> int_of_string in
    Facade.full_transformation_saving_norm ~states_file ~trans_file ~trans2time_shift_file:trans2timeshift_file ~ctrls_file ~module_name:module_name ~output_filename:output_file ~separate_functions:false num_of_agents
  else
    help ()