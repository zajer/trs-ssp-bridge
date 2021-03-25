let help () = 
  print_endline 
  "
  This program generates a source code for generating a walk from an initial state to a desired state.
  
  If you see this message it means that you did not provide necessary number of arguments.
  Arguments required for this program are:
  Arg1 - the name of a file containing discovered states (generated by Trackign_bigraph library).
  Arg2 - a number of agents in the system.
  Arg3 - the name of the source code file (output of this program).
  Arg4 - (optional) the name of a file with template of the source code.
  "
  let () = 
    if Array.length Sys.argv >= 4 then
    let states_file = Sys.argv.(1)
    and noa = Sys.argv.(2) |> int_of_string
    and output_file = Sys.argv.(3) 
    and template_file = if Array.length Sys.argv = 5 then Sys.argv.(4) else "ssp_template.txt" in
        Trs_bridge.Facade.gen_ssp_source ~states_file ~template_file ~source_file:output_file noa
    else
        help ()